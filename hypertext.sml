structure Metatwine =
struct

  type passage_label = string (* any eq type will do *)
  datatype action = Plain
                 | ChangePassage of passage_label
                 | ChangeSelf of (string -> htext)
                 (* | ChangeNode of (string -> htext)
                 | Misc of (unit -> unit) *)
  withtype htext = string * action

  type passage = passage_label * (htext list)
  type story = passage list

  fun colorChanger "red" = ("blue", ChangeSelf colorChanger)
    | colorChanger "blue" = ("green", ChangeSelf colorChanger)
    | colorChanger "green" = ("red", ChangeSelf colorChanger)

  val start_label = "Start"
  val door1_label = "Door Number 1"
  val door2_label = "Door Number 2"
  val start = (start_label, 
    [ ("Welcome to the game! Make a choice: ", Plain),
      ("Door #1", ChangePassage door1_label),
      ("Door #2", ChangePassage door2_label)])
  val door1 = (door1_label,
    [ ("You're wearing a ", Plain),
      ("red", ChangeSelf colorChanger),
      (" dress.", Plain)])

  val door2 = (door2_label,
    [("You picked door 2. Good job, I guess.", Plain)])

  val story = [start, door1, door2]

  (* getPassage : story -> passage_label -> passage option *)
  fun getPassage [] _ = NONE
    | getPassage ((l,p)::ps) label = if label = l then SOME (l,p)
        else getPassage ps label

  (* replaceNode : passage -> string -> htext -> passage *)
  fun replaceNode passage hlabel htext =
    case passage of
         [] => []
       | ((nodelabel, node)::nodes) => 
           if nodelabel = hlabel then htext::nodes
           else (nodelabel,node)::(replaceNode nodes hlabel htext)

  fun plain s = [(s, Plain)]

  (* eval : htext -> passage -> story -> passage *)
  fun eval h (p_id, p) story = 
    case h of
         (s, Plain) => (p_id, p)
       | (s, ChangePassage l) => 
           (case (getPassage story l) of 
                 NONE => ("Unknown", plain "No passage found!") 
               | SOME p' => p')
       | (s, ChangeSelf f) => (p_id, replaceNode p s (f s)) 


  (* evalNth : passage -> int -> story -> passage *)
  (* "click" the nth *non-plaintext* node in a passage *)
  fun evalNth (p_id, nodes) n story =
  let
    fun isRich x = case x of (_, Plain) => false | _ => true
    val rich_nodes = List.filter isRich nodes
    val node = List.nth (rich_nodes, n)
  in
    eval node (p_id, nodes) story
  end

  (* click : passage -> string -> story -> passage *)
  (* "click" by selecting a word to press. clicks on the first instance. *)
  fun click (p_id, nodes) s story =
  let
    val node = valOf (List.find (fn x => case x of (s', a) => s = s') nodes)
  in
    eval node (p_id, nodes) story
  end

  (* render : passage -> string *)
  fun render (p_id, p) =
  let
    fun htextToString (s, a) =
      (case a of
            Plain => s
          | ChangePassage _ => "[["^s^"]]"
          | ChangeSelf _ => "<<"^s^">>")
    fun passageToString pass = String.concatWith " " (map htextToString p)
    val pstring = passageToString p
  in(
    print (p_id ^ "\n");
    print (pstring ^ "\n")
    )end

  
  (*
    val start = "hello a b c d"
  val p1 = "Passage 1"
  val p2 = "Passage 2"
  val p3 = "Passage 3"

  fun render p = print (p ^ "\n")

  (* fun appendText text = *) 

  fun changePassage newPassage =
    render newPassage

  fun go () = 
    render start
    *)
  
end
