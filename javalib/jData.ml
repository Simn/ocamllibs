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

type jpath = (string list) * string

type jversion = int * int (* minor + major *)

(** unqualified names cannot have the characters '.', ';', '[' or '/' *)
type unqualified_name = string

type jwildcard =
  | WExtends (* + *)
  | WSuper (* -  *)
  | WNone

type jtype_argument =
  | TType of jwildcard * jsignature
  | TAny (* * *)

and jsignature =
  | TByte (* B *)
  | TChar (* C *)
  | TDouble (* D *)
  | TFloat (* F *)
  | TInt (* I *)
  | TLong (* J *)
  | TShort (* S *)
  | TBool (* Z *)
  | TObject of jpath * jtype_argument list (* L Classname *)
  | TObjectInner of (string list) * (string * jtype_argument list) list (* L Classname ClassTypeSignatureSuffix *)
  | TArray of jsignature * int option (* [ *)
  | TMethod of jmethod_signature (* ( *)
  | TTypeParameter of string (* T *)

(* ( jsignature list ) ReturnDescriptor (| V | jsignature) *)
and jmethod_signature = jsignature list * jsignature option

(* InvokeDynamic-specific: Method handle *)
type reference_type =
  | RGetField (* constant must be ConstField *)
  | RGetStatic (* constant must be ConstField *)
  | RPutField (* constant must be ConstField *)
  | RPutStatic (* constant must be ConstField *)
  | RInvokeVirtual (* constant must be Method *)
  | RInvokeStatic (* constant must be Method *)
  | RInvokeSpecial (* constant must be Method *)
  | RNewInvokeSpecial (* constant must be Method with name <init> *)
  | RInvokeInterface (* constant must be InterfaceMethod *)

(* TODO *)
type bootstrap_method = int

type jconstant_field = jpath * unqualified_name * jsignature
type jconstant_method = jpath * unqualified_name * jmethod_signature
type jconstant_interface_method = jpath * unqualified_name * jmethod_signature
type jconstant_invoke_dynamic = bootstrap_method * unqualified_name * jsignature

type jconstant =
  (** references a class or an interface - jpath must be encoded as StringUtf8 *)
  | ConstClass of jpath (* tag = 7 *)
  (** field reference *)
  | ConstField of jconstant_field (* tag = 9 *)
  (** method reference; string can be special "<init>" and "<clinit>" values *)
  | ConstMethod of jconstant_method (* tag = 10 *)
  (** interface method reference *)
  | ConstInterfaceMethod of jconstant_interface_method (* tag = 11 *)
  (** constant values *)
  | ConstString of string  (* tag = 8 *)
  | ConstInt of int32 (* tag = 3 *)
  | ConstFloat of float (* tag = 4 *)
  | ConstLong of int64 (* tag = 5 *)
  | ConstDouble of float (* tag = 6 *)
  (** name and type: used to represent a field or method, without indicating which class it belongs to *)
  | ConstNameAndType of unqualified_name * jsignature
  (** UTF8 encoded strings. Note that when reading/writing, take into account Utf8 modifications of java *)
  (* (http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7) *)
  | ConstUtf8 of string
  (** invokeDynamic-specific *)
  | ConstMethodHandle of (reference_type * jconstant) (* tag = 15 *)
  | ConstMethodType of jmethod_signature (* tag = 16 *)
  | ConstInvokeDynamic of jconstant_invoke_dynamic (* tag = 18 *)
  | ConstUnusable

type jaccess_flag =
  | JPublic (* 0x0001 *)
  | JPrivate (* 0x0002 *)
  | JProtected (* 0x0004 *)
  | JStatic (* 0x0008 *)
  | JFinal (* 0x0010 *)
  | JSynchronized (* 0x0020 *)
  | JVolatile (* 0x0040 *)
  | JTransient (* 0x0080 *)
  (** added if created by the compiler *)
  | JSynthetic (* 0x1000 *)
  | JEnum (* 0x4000 *)
  | JUnusable (* should not be present *)
  (** class flags *)
  | JSuper (* 0x0020 *)
  | JInterface (* 0x0200 *)
  | JAbstract (* 0x0400 *)
  | JAnnotation (* 0x2000 *)
  (** method flags *)
  | JBridge (* 0x0040 *)
  | JVarArgs (* 0x0080 *)
  | JNative (* 0x0100 *)
  | JStrict (* 0x0800 *)

type jaccess = jaccess_flag list

(* type parameter name, extends signature, implements signatures *)
type jtypes = (string * jsignature option * jsignature list) list

type jannotation = {
  ann_type : jsignature;
  ann_elements : (string * jannotation_value) list;
}

and jannotation_value =
  | ValConst of jsignature * jconstant (* B, C, D, E, F, I, J, S, Z, s *)
  | ValEnum of jsignature * string (* e *)
  | ValClass of jsignature (* c *) (* V -> Void *)
  | ValAnnotation of jannotation (* @ *)
  | ValArray of jannotation_value list (* [ *)

type jindex = int
type jbyte = int
type jshort = int
type jbranchoffset = int

type jcmp =
  | CmpEq
  | CmpNe
  | CmpLt
  | CmpGe
  | CmpGt
  | CmpLe

type jopcode =
  (* double *)
  | OpD2f
  | OpD2i
  | OpD2l
  | OpDadd
  | OpDaload
  | OpDastore
  | OpDcmpg
  | OpDcmpl
  | OpDdiv
  | OpDconst_0
  | OpDconst_1
  | OpDload of jindex
  | OpDmul
  | OpDneg
  | OpDrem
  | OpDreturn
  | OpDstore of jindex
  | OpDsub
  (* float *)
  | OpF2d
  | OpF2i
  | OpF2l
  | OpFadd
  | OpFaload
  | OpFastore
  | OpFcmpg
  | OpFcmpl
  | OpFdiv
  | OpFconst_0
  | OpFconst_1
  | OpFconst_2
  | OpFload of jindex
  | OpFmul
  | OpFneg
  | OpFrem
  | OpFreturn
  | OpFstore of jindex
  | OPFsub
  (* int *)
  | OpI2b
  | OpI2c
  | OpI2d
  | OpI2f
  | OpI2l
  | OpI2s
  | OpIadd
  | OpIaload
  | OpIand
  | OpIastore
  | OpIconst_m1
  | OpIconst_0
  | OpIconst_1
  | OpIconst_2
  | OpIconst_3
  | OpIconst_4
  | OpIconst_5
  | OpIdiv
  | OpIload of jindex
  | OpImul
  | OpIneg
  | OpIor
  | OpIrem
  | OpIreturn
  | OpIshl
  | OpIshr
  | OpIstore of jindex
  | OpIsub
  | OpIushr
  | OpIxor
  (* long *)
  | OpL2d
  | OpL2f
  | OpL2i
  | OpLadd
  | OpLaload
  | OpLand
  | OpLastore
  | OpLcmp
  | OpLdiv
  | OpLload of jindex
  | OpLmul
  | OpLneg
  | OpLor
  | OpLrem
  | OpLreturn
  | OpLshl
  | OpLshr
  | OpLstore of jindex
  | OpLsub
  | OpLushr
  | OpLxor
  (* short *)
  | OpSaload
  | OpSastore
  | OpSipush of jshort
  (* array *)
  | OpAaload
  | OpAastore
  | OpAnewarray of jpath
  | OpArraylength
  | OpBaload
  | OpBastore
  | OpBipush of jbyte
  | OpCaload
  | OpCastore
  | OpMultianewarray of jpath * jbyte
  | OpNewarray of jsignature (* not really, but might work *)
  (* reference *)
  | OpAload of jindex
  | OpAreturn
  | OpAstore of jindex
  (* object *)
  | OpNew of jpath
  | Opinstanceof of jpath
  | OpCheckcast of jpath
  | OpInvokedynamic of jconstant_invoke_dynamic
  | OpInvokeinterface of jconstant_interface_method * jbyte
  | OpInvokespecial of jconstant_method
  | OpInvokestatic of jconstant_method
  | OpInvokevirtual of jconstant_method
  | OpGetfield of jconstant_field
  | OpGetstatic of jconstant_field
  | OpPutfield of jconstant_field
  | OpPutstatic of jconstant_field
  (* branching *)
  | OpIf_acmpeq of jbranchoffset
  | OpIf_acmpne of jbranchoffset
  | OpIf_icmp of jcmp * jbranchoffset
  | OpIf of jcmp * jbranchoffset
  | OpIfnonnull of jbranchoffset
  | OpIfnull of jbranchoffset
  | OpGoto of jbranchoffset
  | OpGoto_w of jbranchoffset
  | OpJsr of jbranchoffset
  | OpJsr_w of jbranchoffset
  (* stack *)
  | OpAconst_null
  | OpDup
  | OpDup_x1
  | OpDup_x2
  | OpDup2
  | OpDup2_x1
  | OpDup2_x2
  | OpLdc of jindex
  | OpLdc_w of jindex
  | OpLdc2_w of jindex
  | OpNop
  | OpPop
  | OpPop2
  | OpSwap
  (* other *)
  | OpAthrow
  | OpIinc of jindex * jbyte
  | OpLookupswitch (* TODO *)
  | OpMonitorenter
  | OpMonitorexit
  | OpRet of jindex
  | OpReturn
  | OpTableswitch (* TODO *)
  | OpWide (* TODO *)
  (* convenience/custom *)
  | OpIconst of int32
  | OpDconst of float

type jcode = {
  jc_max_stack : int;
  jc_max_locals : int;
  jc_code : jopcode array;
  jc_exception_table : unit array; (* TODO *)
  jc_attributes : jattribute list;
}

and jattribute =
  | AttrDeprecated
  | AttrVisibleAnnotations of jannotation list
  | AttrInvisibleAnnotations of jannotation list
  | AttrCode of jcode
  | AttrUnknown of string * string

type jfield_kind =
  | JKField
  | JKMethod

type jfield = {
  jf_name : string;
  jf_kind : jfield_kind;
  (* signature, as used by the vm *)
  jf_vmsignature : jsignature;
  (* actual signature, as used in java code *)
  jf_signature : jsignature;
  jf_throws : jsignature list;
  jf_types : jtypes;
  jf_flags : jaccess;
  jf_attributes : jattribute list;
  jf_constant : jconstant option;
  jf_code : jcode option;
}

type jclass = {
  cversion : jversion;
  cpath : jpath;
  csuper : jsignature;
  cflags : jaccess;
  cinterfaces : jsignature list;
  cfields : jfield list;
  cmethods : jfield list;
  cattributes : jattribute list;

  cinner_types : (jpath * jpath option * string option * jaccess) list;
  ctypes : jtypes;
}

(* reading/writing *)
type utf8ref = int
type classref = int
type nametyperef = int
type dynref = int
type bootstrapref = int

type jconstant_raw =
  | KClass of utf8ref (* 7 *)
  | KFieldRef of (classref * nametyperef) (* 9 *)
  | KMethodRef of (classref * nametyperef) (* 10 *)
  | KInterfaceMethodRef of (classref * nametyperef) (* 11 *)
  | KString of utf8ref (* 8 *)
  | KInt of int32 (* 3 *)
  | KFloat of float (* 4 *)
  | KLong of int64 (* 5 *)
  | KDouble of float (* 6 *)
  | KNameAndType of (utf8ref * utf8ref) (* 12 *)
  | KUtf8String of string (* 1 *)
  | KMethodHandle of (reference_type * dynref) (* 15 *)
  | KMethodType of utf8ref (* 16 *)
  | KInvokeDynamic of (bootstrapref * nametyperef) (* 18 *)
  | KUnusable

let all_class_flags = [JPublic; JUnusable; JUnusable; JUnusable; JFinal; JSuper; JUnusable; JUnusable; JUnusable; JInterface; JAbstract; JUnusable; JSynthetic; JAnnotation; JEnum]
let all_field_flags = [JPublic; JPrivate; JProtected; JStatic; JFinal; JUnusable; JVolatile; JTransient; JSynthetic; JEnum]
let all_method_flags = [JPublic; JPrivate; JProtected; JStatic; JFinal; JSynchronized; JBridge; JVarArgs; JNative; JUnusable; JAbstract; JStrict; JSynthetic]

(* jData debugging *)
let is_override_attrib = (function
    (* TODO: pass anotations as @:meta *)
    | AttrVisibleAnnotations ann ->
      List.exists (function
        | { ann_type = TObject( (["java";"lang"], "Override"), [] ) } ->
            true
        | _ -> false
      ) ann
    | _ -> false
  )

let is_override field =
  List.exists is_override_attrib field.jf_attributes

let path_s = function
  | (pack,name) -> String.concat "." (pack @ [name])

let rec s_sig = function
  | TByte (* B *) -> "byte"
  | TChar (* C *) -> "char"
  | TDouble (* D *) -> "double"
  | TFloat (* F *) -> "float"
  | TInt (* I *) -> "int"
  | TLong (* J *) -> "long"
  | TShort (* S *) -> "short"
  | TBool (* Z *) -> "bool"
  | TObject(path,args) -> path_s  path ^ s_args args
  | TObjectInner (sl, sjargl) -> String.concat "." sl ^ "." ^ (String.concat "." (List.map (fun (s,arg) -> s ^ s_args arg) sjargl))
  | TArray (s,i) -> s_sig s ^ "[" ^ (match i with | None -> "" | Some i -> string_of_int i) ^ "]"
  | TMethod (sigs, sopt) -> (match sopt with | None -> "" | Some s -> s_sig s ^ " ") ^ "(" ^ String.concat ", " (List.map s_sig sigs) ^ ")"
  | TTypeParameter s -> s

and s_args = function
  | [] -> ""
  | args -> "<" ^ String.concat ", " (List.map (fun t ->
      match t with
      | TAny -> "*"
      | TType (wc, s) ->
        (match wc with
          | WNone -> ""
          | WExtends -> "+"
          | WSuper -> "-") ^
        (s_sig s))
    args) ^ ">"

let s_field f = (if is_override f then "override " else "") ^ s_sig f.jf_signature ^ " " ^ f.jf_name

let s_fields fs = "{ \n\t" ^ String.concat "\n\t" (List.map s_field fs) ^ "\n}"

let s_jcode code =
  let wi s i =
    Printf.sprintf "%s %i" s i
  in
  let wp s path =
    Printf.sprintf "%s %s" s (snd path)
  in
  match code with
  (* double *)
  | OpD2f -> "d2f"
  | OpD2i -> "d2i"
  | OpD2l -> "d2l"
  | OpDadd -> "dadd"
  | OpDaload -> "daload"
  | OpDastore -> "dastore"
  | OpDcmpg -> "dcmpg"
  | OpDcmpl -> "dcmpl"
  | OpDdiv -> "ddiv"
  | OpDconst_0 -> "dconst_0"
  | OpDconst_1 -> "dconst_1"
  | OpDload i -> wi "dload" i
  | OpDmul -> "dmul"
  | OpDneg -> "dneg"
  | OpDrem -> "drem"
  | OpDreturn -> "dreturn"
  | OpDstore i -> wi "dstore" i
  | OpDsub -> "dsub"
  (* float *)
  | OpF2d -> "f2d"
  | OpF2i -> "f2i"
  | OpF2l -> "f2l"
  | OpFadd -> "fadd"
  | OpFaload -> "faload"
  | OpFastore -> "fastore"
  | OpFcmpg -> "fcmpg"
  | OpFcmpl -> "fcmpl"
  | OpFdiv -> "fdiv"
  | OpFconst_0 -> "fconst_0"
  | OpFconst_1 -> "fconst_1"
  | OpFconst_2 -> "fconst_2"
  | OpFload i -> wi "fload" i
  | OpFmul -> "fmul"
  | OpFneg -> "fneg"
  | OpFrem -> "frem"
  | OpFreturn -> "freturn"
  | OpFstore i -> wi "fstore" i
  | OPFsub -> "fsub"
  (* int *)
  | OpI2b -> "i2b"
  | OpI2c -> "i2c"
  | OpI2d -> "i2d"
  | OpI2f -> "i2f"
  | OpI2l -> "i2l"
  | OpI2s -> "i2s"
  | OpIadd -> "iadd"
  | OpIaload -> "iaload"
  | OpIand -> "iand"
  | OpIastore -> "iastore"
  | OpIconst_m1 -> "iconst_m1"
  | OpIconst_0 -> "iconst_0"
  | OpIconst_1 -> "iconst_1"
  | OpIconst_2 -> "iconst_2"
  | OpIconst_3 -> "iconst_3"
  | OpIconst_4 -> "iconst_4"
  | OpIconst_5 -> "iconst_5"
  | OpIdiv -> "idiv"
  | OpIload i -> wi "iload" i
  | OpImul -> "imul"
  | OpIneg -> "ineg"
  | OpIor -> "ior"
  | OpIrem -> "irem"
  | OpIreturn -> "ireturn"
  | OpIshl -> "ishl"
  | OpIshr -> "ishr"
  | OpIstore i -> wi "istore" i
  | OpIsub -> "isub"
  | OpIushr -> "iushr"
  | OpIxor -> "ixor"
  (* long *)
  | OpL2d -> "l2d"
  | OpL2f -> "l2f"
  | OpL2i -> "l2i"
  | OpLadd -> "ladd"
  | OpLaload -> "laload"
  | OpLand -> "land"
  | OpLastore -> "lastore"
  | OpLcmp -> "lcmp"
  | OpLdiv -> "ldiv"
  | OpLload i -> wi "lload" i
  | OpLmul -> "lmul"
  | OpLneg -> "lneg"
  | OpLor -> "lor"
  | OpLrem -> "lrem"
  | OpLreturn -> "lreturn"
  | OpLshl -> "lshl"
  | OpLshr -> "lshr"
  | OpLstore i -> wi "lstore" i
  | OpLsub -> "lsub"
  | OpLushr -> "lushr"
  | OpLxor -> "lxor"
  (* short *)
  | OpSaload -> "saload"
  | OpSastore -> "sastore"
  | OpSipush i -> wi "sipush" i
  (* array *)
  | OpAaload -> "aaload"
  | OpAastore -> "aastore"
  | OpAnewarray path -> wp "anewarray" path
  | OpArraylength -> "arraylength"
  | OpBaload -> "baload"
  | OpBastore -> "bastore"
  | OpBipush i -> wi "bipush" i
  | OpCaload -> "caload"
  | OpCastore -> "castore"
  | OpMultianewarray(path,i) -> "multinewarray" (* TODO *)
  | OpNewarray(jsig) -> "newarray" (* TODO *)
  (* reference *)
  | OpAload i -> wi "aload" i
  | OpAreturn -> "areturn"
  | OpAstore i -> wi "astore" i
  (* object *)
  | OpNew path -> wp "new" path
  | Opinstanceof path -> wp "instanceof" path
  | OpCheckcast path -> wp "checkcast" path
  | OpInvokedynamic arg -> "invokedynamic"
  | OpInvokeinterface(arg1,arg2) -> "invokeinterface"
  | OpInvokespecial arg1 -> "invokespecial"
  | OpInvokestatic arg1 -> "invokestatic"
  | OpInvokevirtual arg1 -> "invokevirtual"
  | OpGetfield arg1 -> "getfield"
  | OpGetstatic arg1 -> "getstatic"
  | OpPutfield arg1 -> "putfield"
  | OpPutstatic arg1 -> "putstatic"
  (* branching *)
  | OpIf_acmpeq i -> wi "acmpeq" i
  | OpIf_acmpne i -> wi "acmpne" i
  | OpIf_icmp(cmp,i) -> wi "icmp" i (* TODO *)
  | OpIf(cmp,i) -> wi "if" i (* TODO *)
  | OpIfnonnull i -> wi "ifnotnull" i
  | OpIfnull i -> wi "ifnull" i
  | OpGoto i -> wi "goto" i
  | OpGoto_w i -> wi "goto_w" i
  | OpJsr i -> wi "jsr" i
  | OpJsr_w i -> wi "jsr_w" i
  (* stack *)
  | OpAconst_null -> "aconst_null"
  | OpDup -> "dup"
  | OpDup_x1 -> "dup_x1"
  | OpDup_x2 -> "dup_x2"
  | OpDup2 -> "dup2"
  | OpDup2_x1 -> "dup2_x1"
  | OpDup2_x2 -> "dup2_x2"
  | OpLdc i -> wi "ldc" i
  | OpLdc_w i -> wi "ldc_w" i
  | OpLdc2_w i -> wi "ldc2_w" i
  | OpNop -> "nop"
  | OpPop -> "pop"
  | OpPop2 -> "pop2"
  | OpSwap -> "swap"
  (* other *)
  | OpAthrow -> "athrow"
  | OpIinc(i1,i2) -> wi "iinc" i1 (* TODO *)
  | OpLookupswitch -> "lookupswitch"
  | OpMonitorenter -> "monitorenter"
  | OpMonitorexit -> "monitorexit"
  | OpRet i -> wi "ret" i
  | OpReturn -> "return"
  | OpTableswitch -> "tableswitch"
  | OpWide -> "wide"
  | OpIconst i -> wi "iconst" (Int32.to_int i)
  | OpDconst f -> Printf.sprintf "dconst %f" f