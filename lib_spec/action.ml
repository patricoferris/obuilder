open Workflow 
open Yaml_util

type ctx = {
  user : Spec.user;
}

let default_ctx = {
  user = Spec.root;
}

type job = { job : Types.job; }

type t = job Types.t 

let job_to_yaml t : Yaml.value = `O [ ("job", Types.job_to_yaml t.job) ]

let cp src dest = Format.(fprintf str_formatter "cp %a %s" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "%s" " ") pp_print_string) src dest); Format.flush_str_formatter ()

let rec of_op acc env ctx (ts : Spec.op list) = match ts with
  | (`Comment _) :: xs -> of_op acc env ctx xs
  | (`Workdir x) :: xs -> of_op ((step |> with_step_run ("cd " ^ x)) :: acc) env ctx xs
  | (`Shell _) :: xs -> of_op acc env ctx xs
  | (`Run { shell; _ }) :: xs -> of_op ((step |> with_step_run shell) :: acc) env ctx xs 
  | (`Copy { src; dst; exclude = _ }) :: xs -> of_op ((step |> with_step_run @@ cp src dst)::acc) env ctx xs 
  | (`User (_ as u)) :: xs -> of_op acc env { user = u } xs 
  | (`Env b) :: xs -> of_op acc ((fst b, `String (snd b)) :: env) ctx xs 
  | [] -> (acc, env, ctx)

let container image = 
  container |> with_image image 

let workflow_of_spec { Spec.from; ops } =
  let (ops', env, _) = of_op [] [] default_ctx ops in
  let on = simple_event ["push"; "pull_request"] in 
  let job = job "ubuntu-latest" |> with_steps (List.rev ops') |> with_job_env (simple_kv env) |> with_container (container from) in 
    t { job } |> with_name "Github Action Workflow" |> with_on on

let to_string t = 
  Pp.workflow ~drop_null:true job_to_yaml Format.str_formatter t;
  Format.flush_str_formatter ()