open Cryptokit
open Core
open Printf

let hex s = transform_string (Hexa.decode()) s
let hexbytes s = Bytes.of_string (hex s)
let tohex s = transform_string (Hexa.encode()) s

let stringifyByte b =
    sprintf "\\x%2.2X" (Char.to_int b)
    
let stringify bytes =
    let ar = String.to_array bytes in
    let s = String.concat_array (Array.map ~f:(fun x -> stringifyByte x) ar) in s
    
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
  let sysArgs = Sys.get_argv() in 
    if (Array.length sysArgs) < 4 then
        printf "%s\n" ("Not enough arguments " ^ usage)
    else
    let iv = "0123456789012345" in
        begin
            match sysArgs.(1) with
            (* | "e" -> printf "%s\n" (stringify (encrypt Sys.argv.(3) Sys.argv.(2) iv))
            | "d" -> printf "%s " Sys.argv.(3); printf "%s\n" (decrypt (Scanf.unescaped Sys.argv.(3)) Sys.argv.(2) iv) *)
            | "e" -> processFile sysArgs.(3) (encrypt sysArgs.(2) iv) 
            | "d" -> processFile sysArgs.(3) (decrypt sysArgs.(2) iv) 
            | _ -> printf "%s\n" usage
        end
