open Core

type t = { major : int; minor : int; patch : int }

(*
type constraint_t = Exact of t
*)

let create ?(major = 0) ?(minor = 0) ?(patch = 0) () =
  let open Result.Monad_infix in
  let verify_positive name i =
    if i < 0 then
      Error (Printf.sprintf "Field \"%s\" cannot be negative, got %d" name i)
    else Ok ()
  in
  verify_positive "major" major >>= fun () ->
  verify_positive "minor" minor >>= fun () ->
  verify_positive "patch" patch >>= fun () -> Ok { major; minor; patch }

module Parser = struct
  type state = Major | Minor of int | Patch of int * int | Done of t

  let parse str =
    let open Result.Monad_infix in
    let str_len = String.length str in
    let c_to_digit_opt c =
      let i = Char.to_int c - 48 in
      if i >= 0 && i <= 9 then Some i else None
    in
    let lex_dot i =
      let next_i = i + 1 in
      (* TODO validate *)
      next_i
    in
    let lex_int idx =
      let rec lex_int_rec prefix idx =
        let ch = String.get str idx in
        match c_to_digit_opt ch with
        | Some i -> (
            if idx + 1 >= str_len then Ok (idx + 1, prefix + i)
            else
              let next_digit_opt = String.get str (idx + 1) |> c_to_digit_opt in
              match next_digit_opt with
              | Some _ ->
                  (* TODO optimize *)
                  lex_int_rec Int.((prefix + i) * 10) (idx + 1)
              | None -> Ok (lex_dot (idx + 1), prefix + i))
        | None -> Error "parse error"
      in
      lex_int_rec 0 idx
    in
    let rec lex_rec idx = function
      | Major ->
          lex_int idx >>= fun (new_idx, major) -> lex_rec new_idx (Minor major)
      | Minor major ->
          lex_int idx >>= fun (new_idx, minor) ->
          lex_rec new_idx (Patch (major, minor))
      | Patch (major, minor) ->
          lex_int idx >>= fun (new_idx, patch) ->
          let t' = { major; minor; patch } in
          lex_rec new_idx (Done t')
      | Done t' -> Ok t'
    in
    lex_rec 0 Major
end

let parse str = Parser.parse str

let is_equal t1 t2 =
  t1.major = t2.major && t1.minor = t2.minor && t1.patch = t2.patch

let to_string t' = Printf.sprintf "%d.%d.%d" t'.major t'.minor t'.patch
