(*
 *  This file is part of JavaLib
 *  Copyright (c)2004-2012 Nicolas Cannasse and Caue Waneck
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)
open JData;;
open IO;;
open IO.BigEndian;;
open ExtString;;
open ExtList;;

exception Writer_error_message of string

let encode_path (pack,name) =
  String.concat "/" (pack @ [name])

let rec encode_param ch param =
  match param with
  | TAny -> write_byte ch (Char.code '*')
  | TType(w, s) ->
    (match w with
    | WExtends -> write_byte ch (Char.code '+')
    | WSuper -> write_byte ch (Char.code '-')
    | WNone -> ());
    encode_sig_part ch s

and encode_sig_part ch jsig = match jsig with
  | TByte -> write_byte ch (Char.code 'B')
  | TChar -> write_byte ch (Char.code 'C')
  | TDouble -> write_byte ch (Char.code 'D')
  | TFloat -> write_byte ch (Char.code 'F')
  | TInt -> write_byte ch (Char.code 'I')
  | TLong -> write_byte ch (Char.code 'J')
  | TShort -> write_byte ch (Char.code 'S')
  | TBool -> write_byte ch (Char.code 'Z')
  | TObject(path, params) ->
    write_byte ch (Char.code 'L');
    nwrite ch (encode_path path);
    if params <> [] then begin
      write_byte ch (Char.code '<');
      List.iter (encode_param ch) params;
      write_byte ch (Char.code '>')
    end;
    write_byte ch (Char.code ';')
  | TObjectInner(pack, inners) ->
    write_byte ch (Char.code 'L');
    List.iter (fun p ->
      nwrite ch p;
      write_byte ch (Char.code '/')
    ) pack;

    let first = ref true in
    List.iter (fun (name,params) ->
      (if !first then first := false else write_byte ch (Char.code '.'));
      nwrite ch name;
      if params <> [] then begin
        write_byte ch (Char.code '<');
        List.iter (encode_param ch) params;
        write_byte ch (Char.code '>')
      end;
    ) inners;
    write_byte ch (Char.code ';')
  | TArray(s,size) ->
    write_byte ch (Char.code '[');
    (match size with
    | Some size ->
      nwrite ch (string_of_int size);
    | None -> ());
    encode_sig_part ch s
  | TMethod(args, ret) ->
    write_byte ch (Char.code '(');
    List.iter (encode_sig_part ch) args;
    write_byte ch (Char.code ')');
    (match ret with
      | None -> write_byte ch (Char.code 'V')
      | Some jsig -> encode_sig_part ch jsig)
  | TTypeParameter name ->
    write_byte ch (Char.code 'T');
    nwrite ch name;
    write_byte ch (Char.code ';')

let encode_sig jsig =
  let buf = IO.output_string() in
  encode_sig_part buf jsig;
  close_out buf

let change_utf8 s =
   if String.contains s (Char.chr 0) then begin
     let buf = Buffer.create (String.length s) in
     String.iter (fun chr ->
       let c = Char.code chr in
       if c = 0 then begin
         Buffer.add_char buf (Char.chr 0xC0);
         Buffer.add_char buf (Char.chr 0x80)
       end else
         Buffer.add_char buf chr) s;
      Buffer.contents buf
   end else
     s
 ;;

let get_reference_type i =
  match i with
  | RGetField ->  1
  | RGetStatic ->  2
  | RPutField ->  3
  | RPutStatic ->  4
  | RInvokeVirtual ->  5
  | RInvokeStatic ->  6
  | RInvokeSpecial ->  7
  | RNewInvokeSpecial ->  8
  | RInvokeInterface ->  9

class jvm_constant_pool = object(self)
  val mutable constants = [];
  val constant_lookup = Hashtbl.create 0;
  val mutable offset = 1;

  method add jc =
    try
      Hashtbl.find constant_lookup jc
    with Not_found ->
      let i = offset in
      constants <- jc :: constants;
      offset <- offset + (match jc with ConstLong _ | ConstDouble _ -> 2 | _ -> 1);
      Hashtbl.add constant_lookup jc i;
      i

  method add_op jc =
    let i = self#add jc in
    match jc with
      | ConstLong _ | ConstDouble _ -> OpLdc2_w i,3
      | _ -> if i <= 0xFF then OpLdc i,2 else OpLdc_w i,3

  method length = offset

  method write_to (ch : string IO.output) =
    let rec loop jc = match jc with
      (** references a class or an interface - jpath must be encoded as StringUtf8 *)
      | ConstClass path -> (* tag = 7 *)
          let arg = (self#add (ConstUtf8 (encode_path path))) in
          write_byte ch 7;
          write_ui16 ch arg;
      (** field reference *)
      | ConstField (jpath, unqualified_name, jsignature) (* tag = 9 *) ->
          let arg1 = (self#add (ConstClass jpath)) in
          let arg2 = (self#add (ConstNameAndType (unqualified_name, jsignature))) in
          write_byte ch 9;
          write_ui16 ch arg1;
          write_ui16 ch arg2;
      (** method reference; string can be special "<init>" and "<clinit>" values *)
      | ConstMethod (jpath, unqualified_name, jmethod_signature) (* tag = 10 *) ->
          let arg1 = (self#add (ConstClass jpath)) in
          let arg2 = (self#add (ConstNameAndType (unqualified_name, TMethod jmethod_signature))) in
          write_byte ch 10;
          write_ui16 ch arg1;
          write_ui16 ch arg2;
      (** interface method reference *)
      | ConstInterfaceMethod (jpath, unqualified_name, jmethod_signature) (* tag = 11 *) ->
          let arg1 = (self#add (ConstClass jpath)); in
          let arg2 = (self#add (ConstNameAndType (unqualified_name, TMethod jmethod_signature))) in
          write_byte ch 11;
          write_ui16 ch arg1;
          write_ui16 ch arg2;
      (** constant values *)
      | ConstString s  (* tag = 8 *) ->
          let arg = (self#add (ConstUtf8 s)) in
          write_byte ch 8;
          write_ui16 ch arg
      | ConstInt i (* tag = 3 *) ->
          write_byte ch 3;
          write_real_i32 ch i
      | ConstFloat f (* tag = 4 *) ->
          write_byte ch 4;
          (match classify_float f with
          | FP_normal | FP_subnormal | FP_zero ->
              write_real_i32 ch (Int32.bits_of_float f)
          | FP_infinite when f > 0.0 ->
              write_real_i32 ch 0x7f800000l
          | FP_infinite ->
              write_real_i32 ch 0xff800000l
          | FP_nan ->
              write_real_i32 ch 0x7f800001l)
      | ConstLong i (* tag = 5 *) ->
          write_byte ch 5;
          write_i64 ch i;
      | ConstDouble d (* tag = 6 *) ->
          write_byte ch 6;
          write_double ch d;
      (** name and type: used to represent a field or method, without indicating which class it belongs to *)
      | ConstNameAndType (unqualified_name, jsignature) ->
          let arg1 = (self#add (ConstUtf8 (unqualified_name))) in
          let arg2 = (self#add (ConstUtf8 (encode_sig jsignature))) in
          write_byte ch 12;
          write_ui16 ch arg1;
          write_ui16 ch arg2;
      (** UTF8 encoded strings. Note that when reading/writing, take into account Utf8 modifications of java *)
      (* (http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7) *)
      | ConstUtf8 s ->
          write_byte ch 1;
          let s = change_utf8 s in
          write_ui16 ch (String.length s);
          nwrite ch s
      (** invokeDynamic-specific *)
      | ConstMethodHandle (reference_type, jconstant) (* tag = 15 *) ->
          let arg = (self#add jconstant) in
          write_byte ch 15;
          write_byte ch (get_reference_type reference_type);
          write_ui16 ch arg;
      | ConstMethodType jmethod_signature (* tag = 16 *) ->
          let arg = (self#add (ConstUtf8 (encode_sig (TMethod jmethod_signature)))) in
          write_byte ch 16;
          write_ui16 ch arg
      | ConstInvokeDynamic (bootstrap_method, unqualified_name, jsignature) (* tag = 18 *) ->
          let arg = (self#add (ConstNameAndType(unqualified_name, jsignature))) in
          write_byte ch 18;
          write_ui16 ch bootstrap_method;
          write_ui16 ch arg
      | ConstUnusable -> assert false
    in
    match constants with
      | [] -> ()
      | l ->
        constants <- [];
        List.iter loop (List.rev l);
        self#write_to ch
end

type context = {
  ch : string IO.output;
  cpool : jvm_constant_pool;
}

let error msg = raise (Writer_error_message msg)

let write_const ctx ch cconst =
  write_ui16 ch (ctx.cpool#add cconst)
;;

let write_formal_type_params ctx ch tparams =
  write_byte ch (Char.code '<');
  List.iter (fun (name,ext,impl) ->
    nwrite ch name;
    (match ext with
    | None -> ()
    | Some jsig ->
      write_byte ch (Char.code ':');
      nwrite ch (encode_sig jsig));
    List.iter (fun jsig ->
      write_byte ch (Char.code ':');
      nwrite ch (encode_sig jsig)
    ) impl
  ) tparams;
  write_byte ch (Char.code '>');
;;

let write_complete_method_signature ctx ch (tparams : jtypes) msig throws =
  if tparams <> [] then write_formal_type_params ctx ch tparams;
  nwrite ch (encode_sig (TMethod(msig)));
  if throws <> [] then List.iter (fun jsig ->
    write_byte ch (Char.code '^');
    nwrite ch (encode_sig jsig)
  ) throws
;;

let write_access_flags ctx all_flags flags =
  let value = List.fold_left (fun acc flag ->
    try
      acc lor (Hashtbl.find all_flags flag)
    with Not_found ->
      error ("Not found flag: " ^ (string_of_int (Obj.magic flag)))
  ) 0 flags in
  write_ui16 ctx.ch value
;;

let rec write_ann_element ctx ch (name,eval) =
  write_const ctx ch (ConstUtf8 name);
  write_element_value ctx ch eval

and write_annotation ctx ch ann =
  write_const ctx ch (ConstUtf8 (encode_sig ann.ann_type));
  write_ui16 ch (List.length ann.ann_elements);
  List.iter (write_ann_element ctx ch) ann.ann_elements

and write_element_value ctx ch value = match value with
  | ValConst(jsig, cconst) -> (match jsig with
    | TObject((["java";"lang"],"String"), []) ->
      write_byte ch (Char.code 's')
    | TByte | TChar | TDouble | TFloat | TInt | TLong | TShort | TBool ->
      nwrite ch (encode_sig jsig)
    | _ ->
      let s = encode_sig jsig in
      error ("Invalid signature " ^ s ^ " for constant value"));
    write_ui16 ch (ctx.cpool#add cconst)
  | ValEnum(jsig,name) ->
    write_byte ch (Char.code 'e');
    write_const ctx ch (ConstUtf8 (encode_sig jsig));
    write_const ctx ch (ConstUtf8 name)
  | ValClass(jsig) ->
    write_byte ch (Char.code 'c');
    let esig = match jsig with
      | TObject(([],"Void"),[])
      | TObject((["java";"lang"],"Void"),[]) ->
        "V"
      | _ ->
        encode_sig jsig
    in
    write_const ctx ch (ConstUtf8 (esig))
  | ValAnnotation ann ->
    write_byte ch (Char.code '@');
    write_annotation ctx ch ann
  | ValArray(lvals) ->
    write_byte ch (Char.code '[');
    write_ui16 ch (List.length lvals);
    List.iter (write_element_value ctx ch) lvals
;;

let write_const_s ctx ch str =
  write_const ctx ch (ConstUtf8 str)
;;

let write_opcode ctx ch code =
  let w = write_byte ch in
  (* TODO: probably don't need these *)
  let bp i =
    w ((i lsr 8) land 0xFF);
    w (i land 0xFF);
  in
  let b4 i =
    w ((i lsr 24) land 0xFF);
    w ((i lsr 16) land 0xFF);
    w ((i lsr 8) land 0xFF);
    w (i land 0xFF);
  in
  let path jpath =
    bp (ctx.cpool#add (ConstClass jpath))
  in
  let meth m =
    bp (ctx.cpool#add (ConstMethod m))
  in
  let field f =
    bp (ctx.cpool#add (ConstField f))
  in
  let rec loop code = match code with
    (* double *)
    | OpD2f -> w 0x90
    | OpD2i -> w 0x8e
    | OpD2l -> w 0x8f
    | OpDadd -> w 0x63
    | OpDaload -> w 0x31
    | OpDastore -> w 0x52
    | OpDcmpg -> w 0x98
    | OpDcmpl -> w 0x97
    | OpDdiv -> w 0x6f
    | OpDconst_0 -> w 0xe
    | OpDconst_1 -> w 0xf
    | OpDload_0 -> w 0x26
    | OpDload_1 -> w 0x27
    | OpDload_2 -> w 0x28
    | OpDload_3 -> w 0x29
    | OpDload i -> w 0x18; w i
    | OpDmul -> w 0x6b
    | OpDneg -> w 0x77
    | OpDrem -> w 0x73
    | OpDreturn -> w 0xaf
    | OpDstore_0 -> w 0x47
    | OpDstore_1 -> w 0x48
    | OpDstore_2 -> w 0x49
    | OpDstore_3 -> w 0x4a
    | OpDstore i -> w 0x39; w i
    | OpDsub -> w 0x67
    (* float *)
    | OpF2d -> w 0x8d
    | OpF2i -> w 0x8b
    | OpF2l -> w 0x8c
    | OpFadd -> w 0x62
    | OpFaload -> w 0x30
    | OpFastore -> w 0x51
    | OpFcmpg -> w 0x96
    | OpFcmpl -> w 0x95
    | OpFdiv -> w 0x6e
    | OpFconst_0 -> w 0xb
    | OpFconst_1 -> w 0xc
    | OpFconst_2 -> w 0xd
    | OpFload_0 -> w 0x22
    | OpFload_1 -> w 0x23
    | OpFload_2 -> w 0x24
    | OpFload_3 -> w 0x25
    | OpFload i -> w 0x17; w i
    | OpFmul -> w 0x6a
    | OpFneg -> w 0x76
    | OpFrem -> w 0x72
    | OpFreturn -> w 0xae
    | OpFstore_0 -> w 0x43
    | OpFstore_1 -> w 0x44
    | OpFstore_2 -> w 0x45
    | OpFstore_3 -> w 0x46
    | OpFstore i -> w 0x38; w i
    | OPFsub -> w 0x66
    (* int *)
    | OpI2b -> w 0x91
    | OpI2c -> w 0x92
    | OpI2d -> w 0x87
    | OpI2f -> w 0x86
    | OpI2l -> w 0x85
    | OpI2s -> w 0x93
    | OpIadd -> w 0x60
    | OpIaload -> w 0x2e
    | OpIand -> w 0x7e
    | OpIastore -> w 0x4f
    | OpIconst_m1 -> w 0x2
    | OpIconst_0 -> w 0x3
    | OpIconst_1 -> w 0x4
    | OpIconst_2 -> w 0x5
    | OpIconst_3 -> w 0x6
    | OpIconst_4 -> w 0x7
    | OpIconst_5 -> w 0x8
    | OpIdiv -> w 0x6c
    | OpIload_0 -> w 0x1a
    | OpIload_1 -> w 0x1b
    | OpIload_2 -> w 0x1c
    | OpIload_3 -> w 0x1d
    | OpIload i -> w 0x15; w i
    | OpImul -> w 0x68
    | OpIneg -> w 0x74
    | OpIor -> w 0x80
    | OpIrem -> w 0x70
    | OpIreturn -> w 0xac
    | OpIshl -> w 0x78
    | OpIshr -> w 0x7a
    | OpIstore_0 -> w 0x3b
    | OpIstore_1 -> w 0x3c
    | OpIstore_2 -> w 0x3d
    | OpIstore_3 -> w 0x3e
    | OpIstore i -> w 0x36; w i
    | OpIsub -> w 0x64
    | OpIushr -> w 0x7c
    | OpIxor -> w 0x82
    (* long *)
    | OpL2d -> w 0x8a
    | OpL2f -> w 0x89
    | OpL2i -> w 0x88
    | OpLadd -> w 0x61
    | OpLaload -> w 0x2f
    | OpLand -> w 0x7f
    | OpLastore -> w 0x50
    | OpLcmp -> w 0x94
    | OpLdiv -> w 0x6d
    | OpLload_0 -> w 0x1e
    | OpLload_1 -> w 0x1f
    | OpLload_2 -> w 0x20
    | OpLload_3 -> w 0x21
    | OpLload i ->  w 0x16; w i
    | OpLmul -> w 0x69
    | OpLneg -> w 0x75
    | OpLor -> w 0x81
    | OpLrem -> w 0x71
    | OpLreturn -> w 0xad
    | OpLshl -> w 0x79
    | OpLshr -> w 0x7b
    | OpLstore_0 -> w 0x3f
    | OpLstore_1 -> w 0x40
    | OpLstore_2 -> w 0x41
    | OpLstore_3 -> w 0x42
    | OpLstore i -> w 0x37; w i
    | OpLsub -> w 0x65
    | OpLushr -> w 0x7d
    | OpLxor -> w 0x83
    (* short *)
    | OpSaload -> w 0x35
    | OpSastore -> w 0x56
    | OpSipush i -> w 0x11; bp i
    (* array *)
    | OpAaload -> w 0x32
    | OpAastore -> w 0x53
    | OpAnewarray jpath -> w 0xbd; path jpath
    | OpArraylength -> w 0xbe
    | OpBaload -> w 0x33
    | OpBastore -> w 0x54
    | OpBipush i -> w 0x10; w i
    | OpCaload -> w 0x34
    | OpCastore -> w 0x55
    | OpMultianewarray(jpath,d) -> w 0xc5; path jpath; w d
    | OpNewarray jsig -> assert false
    (* reference *)
    | OpAload_0 -> w 0x2a
    | OpAload_1 -> w 0x2b
    | OpAload_2 -> w 0x2c
    | OpAload_3 -> w 0x2d
    | OpAload i -> w 0x19; w i
    | OpAreturn -> w 0xb0
    | OpAstore_0 -> w 0x4b
    | OpAstore_1 -> w 0x4c
    | OpAstore_2 -> w 0x4d
    | OpAstore_3 -> w 0x4e
    | OpAstore i -> w 0x3a; w i
    (* object *)
    | OpNew jpath -> w 0xbb; path jpath
    | Opinstanceof jpath -> w 0xc1; path jpath
    | OpCheckcast jpath -> w 0xc0; path jpath
    | OpInvokedynamic id -> w 0xba; bp (ctx.cpool#add (ConstInvokeDynamic id)); w 0; w 0 (* ??? *)
    | OpInvokeinterface(im,c) -> w 0xb9; bp (ctx.cpool#add (ConstInterfaceMethod im)); w c
    | OpInvokespecial m -> w 0xb7; meth m
    | OpInvokestatic m -> w 0xb8; meth m
    | OpInvokevirtual m -> w 0xb6; meth m
    | OpGetfield f -> w 0xb4; field f
    | OpGetstatic f -> w 0xb2; field f
    | OpPutfield f -> w 0xb5; field f
    | OpPutstatic f -> w 0xb3; field f
    (* branching *)
    | OpIf_acmpeq i -> w 0xa5; bp i
    | OpIf_acmpne i -> w 0xa6; bp i
    | OpIf_icmp(cmp,i) ->
      begin match cmp with
        | CmpEq -> w 0x9f
        | CmpNe -> w 0xa0
        | CmpLt -> w 0xa1
        | CmpGe -> w 0xa2
        | CmpGt -> w 0xa3
        | CmpLe -> w 0xa4
      end;
      bp !i
    | OpIf(cmp,i) ->
      begin match cmp with
        | CmpEq -> w 0x99
        | CmpNe -> w 0x9a
        | CmpLt -> w 0x9b
        | CmpGe -> w 0x9c
        | CmpGt -> w 0x9d
        | CmpLe -> w 0x9e
      end;
      bp !i
    | OpIfnonnull i -> w 0xc7; bp i
    | OpIfnull i -> w 0xc6; bp i
    | OpGoto i -> w 0xa7; bp !i
    | OpGoto_w i -> w 0xc8; b4 i
    | OpJsr i -> w 0xa8; bp i
    | OpJsr_w i -> w 0xc9; b4 i
    (* stack *)
    | OpAconst_null -> w 0x1
    | OpDup -> w 0x59
    | OpDup_x1 -> w 0x5a
    | OpDup_x2 -> w 0x5b
    | OpDup2 -> w 0x5c
    | OpDup2_x1 -> w 0x5d
    | OpDup2_x2 -> w 0x5e
    | OpLdc i -> w 0x12; w i
    | OpLdc_w i -> w 0x13; bp i
    | OpLdc2_w i -> w 0x14; bp i
    | OpNop -> w 0x0
    | OpPop -> w 0x57
    | OpPop2 -> w 0x58
    | OpSwap -> w 0x5f
    (* other *)
    | OpAthrow -> w 0xbf
    | OpIinc(i,c) -> w 0x84; w i; w c (* TODO: signed? *)
    | OpLookupswitch -> assert false (* TODO *)
    | OpMonitorenter -> w 0xc2
    | OpMonitorexit -> w 0xc3
    | OpRet i -> w 0xa9; w i
    | OpReturn -> w 0xb1
    | OpTableswitch -> assert false (* TODO *)
    | OpWide -> assert false (* TODO *)
  in
  loop code

open VerificationTypeInfo

let generate_stack_map_table ctx ffl =
  let ch = output_string() in
  write_ui16 ch (List.length ffl);
  let write_verification_type_info vti = match vti with
    | VTITop -> write_byte ch 0
    | VTIInteger -> write_byte ch 1
    | VTIFloat -> write_byte ch 2
    | VTILong -> write_byte ch 4
    | VTIDouble -> write_byte ch 3
    | VTINull -> write_byte ch 5
    | VTIUninitializedThis -> write_byte ch 6
    | VTIObject path -> write_byte ch 7; write_ui16 ch (ctx.cpool#add (ConstClass path))
    | VTIUninitializedVariable offset -> write_byte ch 8; write_ui16 ch offset
  in
  List.iter (fun ff ->
    write_byte ch 255;
    write_ui16 ch ff.ff_offset_delta;
    write_ui16 ch (List.length ff.ff_locals);
    List.iter write_verification_type_info ff.ff_locals;
    write_ui16 ch (List.length ff.ff_stack_items);
    List.iter write_verification_type_info ff.ff_stack_items
  ) ffl;
  close_out ch

let rec generate_code_attribute ctx jcode =
  let ch = output_string() in
  write_ui16 ch jcode.jc_max_stack;
  write_ui16 ch jcode.jc_max_locals;
  let tmp = output_string() in
  Array.iter (write_opcode ctx tmp) jcode.jc_code;
  let s = close_out tmp in
  write_i32 ch (String.length s);
  nwrite ch s;
  write_ui16 ch (Array.length jcode.jc_exception_table);
  (* TODO actual exceptions *)
  write_attributes ctx ch jcode.jc_attributes;
  close_out ch

and write_attribute ctx ch attr =
  let name, contents = match attr with
    | AttrDeprecated ->
      "Deprecated", ""
    | AttrVisibleAnnotations anns
    | AttrInvisibleAnnotations anns ->
      let tmp = IO.output_string() in
      write_ui16 tmp (List.length anns);
      List.iter (write_annotation ctx tmp) anns;
      let out = close_out tmp in
      let name = match attr with
        | AttrVisibleAnnotations _ ->
          "RuntimeVisibleAnnotations"
        | _ ->
          "RuntimeInvisibleAnnotations"
      in
      name, out
    | AttrCode jcode ->
      "Code", generate_code_attribute ctx jcode
    | AttrStackMapTable ffl ->
      "StackMapTable", generate_stack_map_table ctx ffl
    | AttrUnknown(name,contents) ->
      name, contents
  in
  write_const_s ctx ch name;
  write_i32 ch (String.length contents);
  nwrite ch contents

and write_attributes ctx ch attributes =
  write_ui16 ch (List.length attributes);
  List.iter (write_attribute ctx ch) attributes

let enumerated_hashtbl_of_list l =
  let h = Hashtbl.create 0 in
  ExtList.List.iteri (fun i flag -> Hashtbl.add h flag (1 lsl i)) l;
  h

let all_class_flags_table = enumerated_hashtbl_of_list all_class_flags
let all_field_flags_table = enumerated_hashtbl_of_list all_field_flags
let all_method_flags_table = enumerated_hashtbl_of_list all_method_flags

let write_field ctx is_method jf =
  write_access_flags ctx (if is_method then all_method_flags_table else all_field_flags_table) jf.jf_flags;
  write_ui16 ctx.ch (ctx.cpool#add (ConstUtf8 jf.jf_name));
  write_ui16 ctx.ch (ctx.cpool#add (ConstUtf8 (encode_sig jf.jf_vmsignature)));
  let attributes = match jf.jf_code with
    | None -> jf.jf_attributes
    | Some jcode -> (AttrCode jcode) :: jf.jf_attributes
  in
  write_attributes ctx ctx.ch attributes

let write_class ch_main cpool c =
  write_real_i32 ch_main 0xCAFEBABEl;
  write_ui16 ch_main (fst c.cversion);
  write_ui16 ch_main (snd c.cversion);
  let ch = IO.output_string() in
  let ctx = {
    ch = ch;
    cpool = cpool;
  } in
  let index_from_signature jsig = match jsig with
    | TObject(path,[]) -> ctx.cpool#add (ConstClass path)
    | _ -> error "Invalid signature"
  in
  write_access_flags ctx all_class_flags_table c.cflags;
  write_ui16 ch (ctx.cpool#add (ConstClass c.cpath));
  write_ui16 ch (index_from_signature c.csuper);
  write_ui16 ch (List.length c.cinterfaces);
  List.iter (fun jsig -> write_ui16 ch (index_from_signature jsig)) c.cinterfaces;
  write_ui16 ch (List.length c.cfields);
  List.iter (write_field ctx false) c.cfields;
  write_ui16 ch (List.length c.cmethods);
  List.iter (write_field ctx true) c.cmethods;
  write_attributes ctx ctx.ch c.cattributes;
  let ch_cpool = IO.output_string() in
  cpool#write_to ch_cpool;
  write_ui16 ch_main cpool#length;
  nwrite ch_main (close_out ch_cpool);
  nwrite ch_main (close_out ch)