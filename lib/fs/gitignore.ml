module Gitignore = Gitignore

type rule = {
  pattern : string;
  negated : bool;
}

let find_gitignore path =
  let full_path = Filename.concat path ".gitignore" in
  if Sys.file_exists full_path then (
    let ic = open_in full_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some content)
  else None

let parse (content : string) : rule list =
  content
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun line ->
         line <> "" && not (String.starts_with ~prefix:"#" line))
  |> List.map (fun line ->
         if String.starts_with ~prefix:"!" line then
           {
             pattern = String.sub line 1 (String.length line - 1);
             negated = true;
           }
         else { pattern = line; negated = false })

let matches path rule =
  try
    let re = Re.Glob.glob ~anchored:true rule.pattern |> Re.compile in
    Re.execp re path
  with _ -> false

let is_ignored path rules =
  let rec apply rules ignored =
    match rules with
    | [] -> ignored
    | rule :: rest ->
        if matches path rule then apply rest (not rule.negated)
        else apply rest ignored
  in
  apply rules false
