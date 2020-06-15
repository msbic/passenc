open Cryptokit
open Core_kernel
open Printf

let hex s = transform_string (Hexa.decode()) s
let hexbytes s = Bytes.of_string (hex s)
let tohex s = transform_string (Hexa.encode()) s

let stringifyByte b =  sprintf "\\x%2.2X" (Char.to_int b)
    (*match Char.is_print b with
    | true -> String.of_char b;
    | false -> sprintf "\\x%2.2X" (Char.to_int b)*)

let stringify bytes =
    (* printf "%d\n" (String.length bytes); *)
    let ar = String.to_array bytes in
    let s = String.concat_array (Array.map ~f:(fun x -> stringifyByte x) ar) in s
    (* Scanf.unescaped s *)

let encrypt key iv plainText = 
    let aes = Cryptokit.Cipher.(aes ~mode:CBC ~pad:Cryptokit.Padding.length ~iv:iv key Encrypt) in
    stringify (Cryptokit.transform_string aes plainText)

let decrypt key iv cipherText = 
    let aes = Cryptokit.Cipher.(aes ~mode:CBC ~pad:Cryptokit.Padding.length ~iv:iv key Decrypt) in
    Cryptokit.transform_string aes (Scanf.unescaped cipherText)

let usage = "passenc [d|e] key filename"

let processFile filename func =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.iter_lines file ~f:(fun line -> printf "%s\n" (func line)))

let () =
    if (Array.length Sys.argv) < 4 then
        printf "%s" ("Not enough arguments " ^ usage)
    else
    let iv = "0123456789012345" in
        begin
            match Sys.argv.(1) with
            (* | "e" -> printf "%s\n" (stringify (encrypt Sys.argv.(3) Sys.argv.(2) iv))
            | "d" -> printf "%s " Sys.argv.(3); printf "%s\n" (decrypt (Scanf.unescaped Sys.argv.(3)) Sys.argv.(2) iv) *)
            | "e" -> processFile Sys.argv.(3) (encrypt Sys.argv.(2) iv) 
            | "d" -> processFile Sys.argv.(3) (decrypt Sys.argv.(2) iv) 
            | _ -> printf "%s" usage
        end
