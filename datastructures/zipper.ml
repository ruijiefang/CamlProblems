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

(* Navigating the tree using zippers. *)

