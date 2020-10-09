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

let working_dir d st = match d with  
  | Some s -> st |> with_step_workdir s 
  | None -> st 

let rec of_op wd acc env ctx (ts : Spec.op list) = match ts with
  | (`Comment _) :: xs -> of_op wd acc env ctx xs
  | (`Workdir x) :: xs -> of_op (Some x) ((step |> with_step_run ("mkdir -p " ^ x)) :: acc) env ctx xs
  | (`Shell _) :: xs -> of_op wd acc env ctx xs
  | (`Run { shell; _ }) :: xs -> of_op wd ((step |> with_step_run shell |> working_dir wd) :: acc) env ctx xs 
  | (`Copy { src; dst; exclude = _ }) :: xs -> of_op wd ((step |> with_step_run @@ cp src dst)::acc) env ctx xs 
  | (`User (_ as u)) :: xs -> of_op wd acc env { user = u } xs 
  | (`Env b) :: xs -> of_op wd acc ((fst b, `String (snd b)) :: env) ctx xs 
  | [] -> (List.rev acc, env, ctx)

let container image = 
  container 
  |> with_image image 
  |> with_options "--user 1000" 
  |> with_container_env 
    (simple_kv [
      ("HOME", `String "/home/opam");
    ])

let workflow_of_spec { Spec.from; ops } =
  let (ops', env, _) = of_op None [] [] default_ctx ops in
  let on = simple_event ["push"; "pull_request"] in 
  let job = 
    job "ubuntu-latest" 
    |> with_steps ops' 
    |> with_job_env (simple_kv (("HOME", `String "/home/opam") :: env)) 
    |> with_container (container from)
    |> with_job_defaults (with_default_run (run |> with_run_workdir "/home/opam")) in 
    t { job } |> with_name "Github Action Workflow" |> with_on on

let pp ppf t = Pp.workflow ~drop_null:true job_to_yaml ppf t

let to_string t = 
  Pp.workflow ~drop_null:true job_to_yaml Format.str_formatter t;
  Format.flush_str_formatter ()