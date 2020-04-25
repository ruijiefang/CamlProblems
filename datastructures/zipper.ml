(* zipper.ml - a zipper data structure for OCaml. 
 * Author: Ruijie Fang 
 * Reference: G. Huet, Functional Pearl: The Zipper, 
 * J. Func. Prog. 7 (5) 549-554, Sept. 1997.      *)

(* First, we write a zipper structure for a generic
 * tree without fixed arity. We generify Huet's 
 * presentation of a tree using OCaml's 'a generic
 * type. *)

(* Inside our tree, a section is an internal node,
 * leading to more sections. An item is a leaf node,
 * which stores a piece of our generic data. 
 *
 * Note that our tree representation is different 
 * from the usual representation, in which each internal 
 * node has a payload. Here all payloads are in leaf
 * nodes. If we want to e.g. store a payload per node
 * in a t-ary tree, we can make it a t+1-ary tree and
 * store the payload as a leaf node in the extra (or
 * middle) node. *)
type 'a tree = 
  | Item of 'a
  | Section of 'a tree list

(* A path inside a tree is either a path of zero
 * length (In which case, it is 'Top'), or a 
 * triple Node (list of elder siblings, parent, 
 *  list of younger siblings). The parent node
 *  is stored as a path, to allow further traversal:
 *             Top
 *              | path
 *             ...
 *              | path
 *    ...    <- grandparent node -> ...
 *              | path
 *    ...    <-  parent node -> ...
 *              | path
 *   brother <- * current node -> brother ->  ... *)
type 'a path = 
  | Top     (* younger brothers, parent, older brothers *)
  | Node of 'a tree list * 'a path * 'a tree list

(* Each node in a tree can be uniquely represented as
 * a "location" in a tree, which is a pair of the tree
 * it self and a path from that node to root ("Top"). *)
type 'a location = Loc of 'a tree * 'a path

(* We can use the example of arithmetic expressions in 
 * Huet's paper to further illustrate the idea of representing
 * tree objects as location pairs. We represent the parse tree
 * of the arithmetic expression "a*b + c*d" as a tree: 
 *     +----------root----------+ 
 *     |           |            |
 *  +--*--+        *         +--*--+
 *  |  |  |        |         |  |  |
 *  a '*' b       '+'        c '*' d                   *)
let expr_tree : string tree = 
  Section [ Section [ Item "a" ; Item "*" ; Item "b"] ; (* The leftmost child of root. *)
            Item "+"; (* <--- This is a child node of root representing payload. *)
            Section [ Item "c" ; Item "*" ; Item "d" ] (* The rightmost child of root. *)
          ]

(* Using Huet's example, we represent the location of the 
 * second multiplication sign '*' in c*d in the tree as 
 * (drawing w/ current node on top for convenience): 
 *   a--+   c <--- '*' ---> d 
 *      |           | 
 *  '*'-*  <------  *
 *      |           |
 *   b--+          Top                                    *)
let path_mul_2 : string path =
  Node ( [ Item "c" ], 
     (* Parent of multiplication sign is the second-level parent. *)
     Node ([ Section [Item "a" ; Item "*" ; Item "b"] ; Item "+" ], (* younger brothers *)
        Top, (* Our parent is root. *) [] (* No older siblings. *) ), 
       [ Item "d" ] )

(* Now, a location corresponding to the second multiplication 
 * sign in the list is just a pair of (expr_tree, path_mul_2): *)
let loc_mul_2 : string location = Loc (expr_tree, path_mul_2) 

(* Huet Section 2.2: Navigating the tree using zippers. *)

(* Go left on a location defined by (tree, path). *)
let go_left (Loc (t, p)) = 
  match p with 
  | Top -> failwith "Cannot to left on top"
  | Node ([], _, _) -> failwith "Cannot go left of leftmost node."
  | Node (l :: left, up, right) -> Loc (l, Node (left, up, t :: right))

(* Go right on a location defined by (tree, path). *)
let go_right (Loc (t, p)) =
  match p with 
  | Top -> failwith "Cannot to right on top"
  | Node (_, _, []) -> failwith "Cannot go right of rightmost node."
  | Node (left, up, r :: right) -> Loc (r, Node (t :: left, up, right))

(* Go up a layer to the parent on location defined by (tree, path). *)
(* This is, yet, the most subtle operation in this data structure. 
 * First, we have to realize that since ocaml lists are singly linked,
 * going right has the effect of "pushing" left nodes onto the left list
 * like a stack, so the left list is actually LIFO ordered when traversing
 * the children of a parent. We can, of course, choose FIFO '@' linking over
 * LIFO appending, but that would cost us time linear in the left list 
 * every time we go right. Not a good solution. So here we have to reverse
 * the ordering of the left list. If we implement something like a doubly linked
 * list this problem will be solved and `go_up` can work in real constant time.
 *
 * This is in general not a problem when the arity of the tree is very small.
 * 
 * The success of zipper also partly comes from the observation that we can
 * use the existing local information (of sibling nodes) to build up the parent
 * node as we go up. Therefore there is no loss of information at all when we
 * traverse subtrees. *)
let go_up (Loc (t, p)) = 
  match p with 
  | Top -> failwith "Cannot go up further; already top."
  | Node (left, up, right) -> Loc (Section ((List.rev left) @ (t :: right)), up)

(* Goes down a node in the tree. *)
let go_down (Loc (t, p)) = 
  match p with 
  | Item _ -> failwith "Cannot go down to an item (leaf) node"
  | Section t :: trees -> Loc (t, Node ([], p, trees))

(* Next, a helper method to access the n-th child of a subtree. *)
let rec nth loc = function
  | 1 -> go_down loc
  | n -> if n > 0 then go_right @@ nth loc (n - 1) else failwith "nth: n <= 0"

