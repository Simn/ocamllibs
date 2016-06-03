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

type context = {
  cpool : string IO.output;
  ch : string IO.output;
  mutable ccount : int;
  mutable constants : (jconstant,int) PMap.t;
}

let error msg = raise (Writer_error_message msg)

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

let encode_path ctx (pack,name) =
  String.concat "/" (pack @ [name])

let rec encode_param ctx ch param =
  match param with
  | TAny -> write_byte ch (Char.code '*')
  | TType(w, s) ->
    (match w with
    | WExtends -> write_byte ch (Char.code '+')
    | WSuper -> write_byte ch (Char.code '-')
    | WNone -> ());
    encode_sig_part ctx ch s

and encode_sig_part ctx ch jsig = match jsig with
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
    nwrite ch (encode_path ctx path);
    if params <> [] then begin
      write_byte ch (Char.code '<');
      List.iter (encode_param ctx ch) params;
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
        List.iter (encode_param ctx ch) params;
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
    encode_sig_part ctx ch s
  | TMethod(args, ret) ->
    write_byte ch (Char.code '(');
    List.iter (encode_sig_part ctx ch) args;
    write_byte ch (Char.code ')');
    (match ret with
      | None -> write_byte ch (Char.code 'V')
      | Some jsig -> encode_sig_part ctx ch jsig)
  | TTypeParameter name ->
    write_byte ch (Char.code 'T');
    nwrite ch name;
    write_byte ch (Char.code ';')

let encode_sig ctx jsig =
  let buf = IO.output_string() in
  encode_sig_part ctx buf jsig;
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

let rec const ctx c =
  try
    PMap.find c ctx.constants
  with
  | Not_found ->
    (match c with
    (** references a class or an interface - jpath must be encoded as StringUtf8 *)
    | ConstClass path -> (* tag = 7 *)
        let arg = (const ctx (ConstUtf8 (encode_path ctx path))) in
        write_byte ctx.cpool 7;
        write_ui16 ctx.cpool arg;
    (** field reference *)
    | ConstField (jpath, unqualified_name, jsignature) (* tag = 9 *) ->
        let arg1 = (const ctx (ConstClass jpath)) in
        let arg2 = (const ctx (ConstNameAndType (unqualified_name, jsignature))) in
        write_byte ctx.cpool 9;
        write_ui16 ctx.cpool arg1;
        write_ui16 ctx.cpool arg2;
    (** method reference; string can be special "<init>" and "<clinit>" values *)
    | ConstMethod (jpath, unqualified_name, jmethod_signature) (* tag = 10 *) ->
        let arg1 = (const ctx (ConstClass jpath)) in
        let arg2 = (const ctx (ConstNameAndType (unqualified_name, TMethod jmethod_signature))) in
        write_byte ctx.cpool 10;
        write_ui16 ctx.cpool arg1;
        write_ui16 ctx.cpool arg2;
    (** interface method reference *)
    | ConstInterfaceMethod (jpath, unqualified_name, jmethod_signature) (* tag = 11 *) ->
        let arg1 = (const ctx (ConstClass jpath)); in
        let arg2 = (const ctx (ConstNameAndType (unqualified_name, TMethod jmethod_signature))) in
        write_byte ctx.cpool 11;
        write_ui16 ctx.cpool arg1;
        write_ui16 ctx.cpool arg2;
    (** constant values *)
    | ConstString s  (* tag = 8 *) ->
        let arg = (const ctx (ConstUtf8 s)) in
        write_byte ctx.cpool 8;
        write_ui16 ctx.cpool arg
    | ConstInt i (* tag = 3 *) ->
        write_byte ctx.cpool 3;
        write_real_i32 ctx.cpool i
    | ConstFloat f (* tag = 4 *) ->
        write_byte ctx.cpool 4;
        (match classify_float f with
        | FP_normal | FP_subnormal | FP_zero ->
            write_real_i32 ctx.cpool (Int32.bits_of_float f)
        | FP_infinite when f > 0.0 ->
            write_real_i32 ctx.cpool 0x7f800000l
        | FP_infinite ->
            write_real_i32 ctx.cpool 0xff800000l
        | FP_nan ->
            write_real_i32 ctx.cpool 0x7f800001l)
    | ConstLong i (* tag = 5 *) ->
        write_byte ctx.cpool 5;
        write_i64 ctx.cpool i;
    | ConstDouble d (* tag = 6 *) ->
        write_byte ctx.cpool 6;
        write_double ctx.cpool d;
        ctx.ccount <- ctx.ccount + 1
    (** name and type: used to represent a field or method, without indicating which class it belongs to *)
    | ConstNameAndType (unqualified_name, jsignature) ->
        let arg1 = (const ctx (ConstUtf8 (unqualified_name))) in
        let arg2 = (const ctx (ConstUtf8 (encode_sig ctx jsignature))) in
        write_byte ctx.cpool 12;
        write_ui16 ctx.cpool arg1;
        write_ui16 ctx.cpool arg2;
    (** UTF8 encoded strings. Note that when reading/writing, take into account Utf8 modifications of java *)
    (* (http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7) *)
    | ConstUtf8 s ->
        write_byte ctx.cpool 1;
        let s = change_utf8 s in
        write_ui16 ctx.cpool (String.length s);
        nwrite ctx.cpool s
    (** invokeDynamic-specific *)
    | ConstMethodHandle (reference_type, jconstant) (* tag = 15 *) ->
        let arg = (const ctx jconstant) in
        write_byte ctx.cpool 15;
        write_byte ctx.cpool (get_reference_type reference_type);
        write_ui16 ctx.cpool arg;
    | ConstMethodType jmethod_signature (* tag = 16 *) ->
        let arg = (const ctx (ConstUtf8 (encode_sig ctx (TMethod jmethod_signature)))) in
        write_byte ctx.cpool 16;
        write_ui16 ctx.cpool arg
    | ConstInvokeDynamic (bootstrap_method, unqualified_name, jsignature) (* tag = 18 *) ->
        let arg = (const ctx (ConstNameAndType(unqualified_name, jsignature))) in
        write_byte ctx.cpool 18;
        write_ui16 ctx.cpool bootstrap_method;
        write_ui16 ctx.cpool arg
    | ConstUnusable -> assert false);
    ctx.ccount <- ctx.ccount + 1;
    ctx.ccount - 1

let write_const ctx ch cconst =
  write_ui16 ch (const ctx cconst)
;;

let write_formal_type_params ctx ch tparams =
  write_byte ch (Char.code '<');
  List.iter (fun (name,ext,impl) ->
    nwrite ch name;
    (match ext with
    | None -> ()
    | Some jsig ->
      write_byte ch (Char.code ':');
      nwrite ch (encode_sig ctx jsig));
    List.iter (fun jsig ->
      write_byte ch (Char.code ':');
      nwrite ch (encode_sig ctx jsig)
    ) impl
  ) tparams;
  write_byte ch (Char.code '>');
;;

let write_complete_method_signature ctx ch (tparams : jtypes) msig throws =
  if tparams <> [] then write_formal_type_params ctx ch tparams;
  nwrite ch (encode_sig ctx (TMethod(msig)));
  if throws <> [] then List.iter (fun jsig ->
    write_byte ch (Char.code '^');
    nwrite ch (encode_sig ctx jsig)
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
  write_const ctx ch (ConstUtf8 (encode_sig ctx ann.ann_type));
  write_ui16 ch (List.length ann.ann_elements);
  List.iter (write_ann_element ctx ch) ann.ann_elements

and write_element_value ctx ch value = match value with
  | ValConst(jsig, cconst) -> (match jsig with
    | TObject((["java";"lang"],"String"), []) ->
      write_byte ch (Char.code 's')
    | TByte | TChar | TDouble | TFloat | TInt | TLong | TShort | TBool ->
      nwrite ch (encode_sig ctx jsig)
    | _ ->
      let s = encode_sig ctx jsig in
      error ("Invalid signature " ^ s ^ " for constant value"));
    write_ui16 ch (const ctx cconst)
  | ValEnum(jsig,name) ->
    write_byte ch (Char.code 'e');
    write_const ctx ch (ConstUtf8 (encode_sig ctx jsig));
    write_const ctx ch (ConstUtf8 name)
  | ValClass(jsig) ->
    write_byte ch (Char.code 'c');
    let esig = match jsig with
      | TObject(([],"Void"),[])
      | TObject((["java";"lang"],"Void"),[]) ->
        "V"
      | _ ->
        encode_sig ctx jsig
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

let write_attribute ctx attr =
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
    | AttrUnknown(name,contents) ->
      name, contents
  in
  write_const_s ctx ctx.ch name;
  write_i32 ctx.ch (String.length contents);
  nwrite ctx.ch contents

let write_attributes ctx attributes =
  write_ui16 ctx.ch (List.length attributes);
  List.iter (write_attribute ctx) attributes

let enumerated_hashtbl_of_list l =
  let h = Hashtbl.create 0 in
  ExtList.List.iteri (fun i flag -> Hashtbl.add h flag (1 lsl i)) l;
  h

let all_class_flags_table = enumerated_hashtbl_of_list all_class_flags
let all_field_flags_table = enumerated_hashtbl_of_list all_field_flags
let all_method_flags_table = enumerated_hashtbl_of_list all_method_flags

let write_field ctx is_method jf =
  write_access_flags ctx (if is_method then all_method_flags_table else all_field_flags_table) jf.jf_flags;
  write_ui16 ctx.ch (const ctx (ConstUtf8 jf.jf_name));
  write_ui16 ctx.ch (const ctx (ConstUtf8 (encode_sig ctx jf.jf_vmsignature)));
  write_attributes ctx jf.jf_attributes

let write_class ch_main c =
  write_real_i32 ch_main 0xCAFEBABEl;
  write_ui16 ch_main (fst c.cversion);
  write_ui16 ch_main (snd c.cversion);
  let ch = IO.output_string() in
  let ctx = {
    ccount = 1;
    cpool = IO.output_string();
    ch = ch;
    constants = PMap.empty;
  } in
  let index_from_signature jsig = match jsig with
    | TObject(path,[]) -> const ctx (ConstClass path)
    | _ -> error "Invalid signature"
  in
  write_access_flags ctx all_class_flags_table c.cflags;
  write_ui16 ch (const ctx (ConstClass c.cpath));
  write_ui16 ch (index_from_signature c.csuper);
  write_ui16 ch (List.length c.cinterfaces);
  List.iter (fun jsig -> write_ui16 ch (index_from_signature jsig)) c.cinterfaces;
  write_ui16 ch (List.length c.cfields);
  List.iter (write_field ctx false) c.cfields;
  write_ui16 ch (List.length c.cmethods);
  List.iter (write_field ctx true) c.cmethods;
  write_attributes ctx c.cattributes;
  write_ui16 ch_main ctx.ccount;
  nwrite ch_main (close_out ctx.cpool);
  nwrite ch_main (close_out ch)