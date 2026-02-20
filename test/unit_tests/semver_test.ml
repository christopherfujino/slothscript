open OUnit2
open Core

let tests =
  let open Sloth_common.Semver in
  let open Result.Monad_infix in
  [
    ( "Semver.create" >:: fun _ ->
      create ~major:0 ~minor:1 ~patch:0 ()
      >>= (fun t' ->
      assert_equal t'.major 0;
      assert_equal t'.minor 1;
      assert_equal t'.patch 0;
      Ok ())
      |> Result.ok_or_failwith );
    ( "Semver.is_equal" >:: fun _ ->
      create ~major:2 ~minor:0 ~patch:0 ()
      >>= (fun t1 ->
      create ~major:2 ~minor:0 ~patch:0 () >>= fun t2 ->
      create ~major:2 ~minor:0 ~patch:1 () >>= fun t3 ->
      assert_bool "is_equal" (is_equal t1 t2);
      assert_bool "is_equal" (not (is_equal t1 t3));
      Ok ())
      |> Result.ok_or_failwith );
    ( "Semver.parse" >:: fun _ ->
      parse "4.0.123"
      >>= (fun from_parse ->
      create ~major:4 ~minor:0 ~patch:123 () >>= fun from_create ->
      Ok
        (assert_bool
           (Printf.sprintf ".parse -> %s\t.create -> %s" (to_string from_parse)
              (to_string from_create))
           (is_equal from_parse from_create)))
      |> Result.ok_or_failwith );
  ]
