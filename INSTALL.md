# Installation and Testing Guide

This guide provides step-by-step instructions to install dependencies, build, and test the our project.


## System Requirements

- **Operating System**: macOS, Linux, or WSL2 on Windows
- **OCaml**: Version 4.14.0 or later
- **OPAM**: OCaml package manager (version 2.0+)
- **SDL2**: Required for Bogue GUI (graphics library)

## Dependencies

### System-Level Dependencies

#### macOS (using Homebrew)
```bash
brew install sdl2 sdl2_image sdl2_ttf sdl2_mixer
```

#### Ubuntu/Debian Linux
```bash
sudo apt-get update
sudo apt-get install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev libsdl2-mixer-dev
```

### OCaml Dependencies

The project requires the following OCaml packages (installed via OPAM):

`dune` >= 3.20
`ocaml` >= 4.14
`bogue` latest
`lwt` latest
`lwt_ppx` latest
`ounit2` latest

## Installation Steps

### 1. Install OPAM (if not already installed)

#### macOS
```bash
brew install opam
```

#### Linux
```bash
# Ubuntu/Debian
sudo apt-get install opam
```

### 2. Install OCaml Dependencies

```bash
# Install all required packages
opam install dune bogue lwt lwt_ppx ounit2

### 4. Clone and Navigate to Project

```bash
cd /Users/name/developer/cs3110/cs3110-project
# or wherever your project is located
```

## Building the Project

### Clean Build

```bash
# Clean previous builds (if any)
dune clean

# Build the project
dune build

# Expected output:
# ld: warning: ignoring duplicate libraries: '-lSDL2'  (this is harmless)
```

## Testing Real-Time Collaboration

This is the primary way to test the system's functionality.

### Step 1: Start the Server

Open **Terminal 1**:

```bash
dune exec server
```

**Expected Output:**
```
Starting server on 127.0.0.1:9001...
Server started on 127.0.0.1:9001
Press Ctrl-C to stop the server
```

**Keep this terminal open.** The server is now listening for client connections.

### Step 2: Start First Client

Open **Terminal 2**:

```bash
dune exec client
```

**Expected Output:**
```
Starting GUI client...
Connecting to server...
Connected!
Loading Bogue 20250815 with config dir /Users/username/.opam/cs3110-2025fa/share/bogue/themes/default
2025-11-19 XX:XX:XX.XXX team_proj[XXXX:XXXXXX] INFO: Using SDL 2.32.10
```

**A GUI window should appear** with:
- Status label: "Connected to server"
- Large text display area (shows shared document)
- Input field with placeholder text
- Three buttons: Send Text, Backspace, Save

### Step 3: Start Second Client (Optional)

Open **Terminal 3**:

```bash
dune exec client
```

Another GUI window will open. Now you have two clients editing the same document!

### Step 4: Test Collaborative Editing

#### Test 1: Basic Text Entry
1. In **Client 1**: Type "hello" in the input field
2. Click **Send Text**
3. **Verify**: Text "hello" appears in both Client 1 and Client 2 text displays

#### Test 2: Multi-Client Editing
1. In **Client 2**: Type " world" in the input field
2. Click **Send Text**
3. **Verify**: Both clients now show "hello world"

#### Test 3: Backspace
1. In **Client 1**: Click **Backspace** button 5 times
2. **Verify**: Both clients show "hello " (removed "world")
(Note: Backspace only counts one input even if its held down)

#### Test 4: Save Request
1. In any client: Click **Save**
2. **Verify**: Status label changes to "Save requested!"
3. **Note**: Actual save functionality depends on backend implementation

#### Test 5: Multiple Clients
1. Start a third client in Terminal 4
2. **Verify**: New client receives full document ("hello ")
3. Type in any client
4. **Verify**: All three clients update simultaneously

### Step 5: Shutdown

#### Stop Server
In **Terminal 1** (server), press:
```
Ctrl-C
```

**Expected Output:**
```
^C
Received Ctrl-C, shutting down server...
Server stopped.
```

#### Close Clients
Simply close the GUI windows or press Ctrl-C in their terminals.

