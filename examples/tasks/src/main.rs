mod messages;

use std::sync::{Arc, Mutex};
use std::time::SystemTime;

use axum::extract::ws::{Message, WebSocket, WebSocketUpgrade};
use axum::routing::get;
use axum::Router;
use tower_http::services::ServeDir;

use elm_wire3_rs::wire3::{Wire3Decoder, Wire3Encoder};
use messages::{Task, ToBackend, ToFrontend};

struct AppState {
    tasks: Mutex<Vec<Task>>,
    next_id: Mutex<i64>,
}

fn timestamp() -> String {
    let secs = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    let h = (secs / 3600) % 24;
    let m = (secs / 60) % 60;
    let s = secs % 60;
    format!("{h:02}:{m:02}:{s:02}")
}

fn log_recv(msg: &ToBackend, bytes: usize) {
    println!("[{}] \x1b[36m<- {:<45}\x1b[0m [{} bytes]", timestamp(), format!("{msg:?}"), bytes);
}

fn log_send(msg: &ToFrontend, bytes: usize) {
    println!("[{}] \x1b[32m-> {:<45}\x1b[0m [{} bytes]", timestamp(), format!("{msg:?}"), bytes);
}

fn encode(msg: &ToFrontend) -> Vec<u8> {
    let mut enc = Wire3Encoder::new();
    msg.wire3_encode(&mut enc);
    let bytes = enc.into_bytes();
    log_send(msg, bytes.len());
    bytes
}

#[tokio::main]
async fn main() {
    let state = Arc::new(AppState {
        tasks: Mutex::new(Vec::new()),
        next_id: Mutex::new(1),
    });

    let app = Router::new()
        .route(
            "/ws",
            get({
                let state = Arc::clone(&state);
                move |ws: WebSocketUpgrade| async move {
                    ws.on_upgrade(move |socket| handle_ws(socket, state))
                }
            }),
        )
        .fallback_service(ServeDir::new("static"));

    println!("\n\x1b[1m  Wire3 Task Manager\x1b[0m");
    println!("  http://localhost:3000\n");
    println!("  Waiting for WebSocket connections...\n");

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

async fn handle_ws(mut socket: WebSocket, state: Arc<AppState>) {
    println!("[{}] \x1b[33mClient connected\x1b[0m", timestamp());

    while let Some(Ok(msg)) = socket.recv().await {
        match msg {
            Message::Binary(data) => {
                let mut dec = Wire3Decoder::new(&data);
                match ToBackend::wire3_decode(&mut dec) {
                    Ok(to_backend) => {
                        log_recv(&to_backend, data.len());
                        let responses = handle_message(&state, to_backend);
                        for resp in responses {
                            let bytes = encode(&resp);
                            if socket.send(Message::Binary(bytes.into())).await.is_err() {
                                return;
                            }
                        }
                    }
                    Err(e) => {
                        println!("[{}] \x1b[31mDecode error: {e}\x1b[0m", timestamp());
                    }
                }
            }
            Message::Close(_) => break,
            _ => {}
        }
    }

    println!("[{}] \x1b[33mClient disconnected\x1b[0m", timestamp());
}

fn handle_message(state: &AppState, msg: ToBackend) -> Vec<ToFrontend> {
    match msg {
        ToBackend::RequestTasks => {
            let tasks = state.tasks.lock().unwrap().clone();
            vec![ToFrontend::TaskList(tasks)]
        }
        ToBackend::AddTask(title) => {
            let id = {
                let mut next = state.next_id.lock().unwrap();
                let id = *next;
                *next += 1;
                id
            };
            let task = Task {
                id: elm_wire3_rs::wire3::types::ElmInt::new(id).unwrap(),
                title,
                completed: false,
            };
            state.tasks.lock().unwrap().push(task.clone());
            vec![ToFrontend::TaskAdded(task)]
        }
        ToBackend::ToggleTask(id) => {
            let mut tasks = state.tasks.lock().unwrap();
            if let Some(task) = tasks.iter_mut().find(|t| t.id == id) {
                task.completed = !task.completed;
                vec![ToFrontend::TaskToggled(id, task.completed)]
            } else {
                vec![]
            }
        }
        ToBackend::DeleteTask(id) => {
            let mut tasks = state.tasks.lock().unwrap();
            tasks.retain(|t| t.id != id);
            vec![ToFrontend::TaskDeleted(id)]
        }
    }
}
