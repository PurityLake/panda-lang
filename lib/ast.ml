open Tokenizer

type ast =
  | Node of ast * ast * token
  | Leaf of token
  | NullLeaf

let left tree =
  match tree with 
  | Node(left, _, _) -> Some(left)
  | _                -> None

let right tree =
  match tree with
  | Node(_, right, _) -> Some(right)
  | _                 -> None

let value tree =
  match tree with
  | Node(_, _, token) -> Some(token)
  | Leaf(token)       -> Some(token)
  | _                 -> None