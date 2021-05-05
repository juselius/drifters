module Server

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks.V2
open Saturn
open Giraffe
open Serilog

open Shared
open Settings

type Storage() =
    let todos = ResizeArray<_>()

    member __.GetTodos() = List.ofSeq todos

    member __.AddTodo(todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

let storage = Storage()

storage.AddTodo(Todo.create "Create new SAFE project")
|> ignore

storage.AddTodo(Todo.create "Write your app")
|> ignore

storage.AddTodo(Todo.create "Ship it !!!")
|> ignore

let private getTodos next ctx =
    task {
        let todos = storage.GetTodos ()
        return! json todos next ctx
    }

let private addTodo (next: HttpFunc) (ctx: HttpContext) =
    task {
        let! todo = ctx.BindJsonAsync<Todo> ()
        match storage.AddTodo todo with
        | Ok () -> return! json todo next ctx
        | Error e -> return! RequestErrors.BAD_REQUEST "fail" next ctx
    }

let webApp =
    choose [
        GET >=> route "/api/getTodos" >=> getTodos
        POST >=> route "/api/addTodo" >=> addTodo
    ]

let configureSerilog () =
    LoggerConfiguration()
        .MinimumLevel.Information()
        .WriteTo.Console()
        .CreateLogger()

let serilog logger =
    logger
        .SetMinimumLevel(LogLevel.Information)
        .AddSerilog() |> ignore

let app =
    Log.Logger <- configureSerilog ()

    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
        use_gzip
        logging serilog
    }

run app
