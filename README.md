# OCaml Collaborative Text Editor

A real-time, multi-threaded collaborative text editor built in OCaml. This application allows multiple users to connect to a central server, edit a shared document simultaneously with live character updates, and see the cursor positions of other users in real-time.

## 🚀 Features

Real-Time Collaboration: Character-by-character updates are broadcast immediately to all connected clients.

Live Synchronization: Implements Operational Transformation (OT) logic to shift user cursors dynamically when text is inserted or deleted by others, keeping everyone in sync.

Visual Formatting: Supports text alignment (Left, Center, Right) and text wrapping.

Persistence: Server-side file saving (via Ctrl+S or UI button) ensures data is preserved in data/saveddoc.txt.

Robust Networking: Built on Lwt for asynchronous, non-blocking TCP communication.

Graphical Interface: Custom UI built using the Bogue library.

## 🛠 Architecture

The project follows a strict Client-Server architecture with a modular codebase designed for testability.

The Backend (lib/backend)

Logic vs. Execution: The backend is split into pure logic (server.ml) and execution loops (run_server.ml). This allows us to unit test the server's state machine without spinning up actual network sockets.

Document Engine (document.ml): A dedicated module that acts as the "Brain." It handles the raw string manipulation and the mathematical logic for shifting cursors (the Synchronization Engine).

Type Safety: Uses strict .mli interfaces to enforce encapsulation (Abstract Data Types) and Representation Invariants.

The Frontend (lib/frontend, lib/spec_actions)

Model-View Separation: Text data is stored with its style attributes (Model) separate from the Bogue widgets (View).

Input Polling: A high-frequency polling loop detects changes in the text input field to simulate a "live typing" experience.

## 📦 Requirements

OCaml (4.14.0 or later)

Opam (Package Manager)

Libraries:

bogue (GUI)

lwt, lwt_ppx, lwt.unix (Concurrency)

ounit2 (Testing)

bisect_ppx (Code Coverage)

## 🚀 Getting Started

1. Build the Project

dune build


2. Run the Server

Open a terminal and start the server. This listens on 127.0.0.1:9000.

dune exec bin/run_server.exe
### OR
dune exec server


Note: The server will automatically create a data/ directory to store saved files if it doesn't exist.

3. Run the Client(s)

Open a new terminal (keep the server running!) and start a client.

dune exec bin/main.exe
### OR
dune exec client


You can open multiple terminals and run multiple clients to test collaboration.

## 🧪 Testing & Coverage

We prioritize stability in our synchronization logic. The project includes a comprehensive test suite targeting the backend logic, protocol parsing, and cursor mathematics.

Run Unit Tests

dune runtest


Generate Coverage Report

We use bisect_ppx to ensure our tests cover >80% of the critical backend logic.

dune clean
dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html


Open _coverage/index.html to view the report.

## 📂 Project Structure

├── bin/
│   ├── main.ml         # Client Entry Point (GUI)
│   └── run_server.ml   # Server Entry Point (Network Loops)
├── lib/
│   ├── backend/
│   │   ├── document.ml # Core Logic (Sync & Cursor Math)
│   │   └── server.ml   # Server State & Protocol Logic
│   ├── frontend/       # GUI Layout & Networking
│   └── spec_actions.ml # Frontend Data Structures
├── test/
│   ├── test_team_proj.ml # Main Logic Tests
│   └── test_server.ml    # Protocol & Parsing Tests
└── data/                 # Storage for saved documents


## 👥 Contributors

Muhammad Ali: Backend Architecture, Synchronization Logic (OT), Persistence, Testing Infrastructure.

Stanley Amkhanitsky: Text Rendering, Alignment Logic, Wrapping implementation.

Ratchaphon Lertdamrongwong: Live Input Polling, Frontend UX optimization, Dirty-flag state management.

Jacob Kupperman: UI Styling, Project Management, Frontend Refactoring.
