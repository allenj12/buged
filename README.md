# Buged - A minimal editor

![bugedDemo2](https://github.com/user-attachments/assets/b10de3d0-4fd2-4431-9084-b56d2e797c74)

## Features
* Minimal UI - just the text
* UTF8/Unicode support
* Shell execution in editor and on text
* Scheme code execution
* Live customization with chez scheme
* Copy/Paste shared with OS clipboard if using (pbpaste & pbcopy)/xclip/(wl-copy & wl-paste)
* Rainbow parens
* Mouse support
* Minimal dependencies - editor itself uses termios for display.
    * ncurses is used for chez scheme but is removable if chez scheme is rebuilt without it, and not using interactive expeditor features (repl)

## Usage
Commands that require some sort of text input require the section of that text to execute.

For example if you want to jump to line number '10':

**ctrl+h** (set the mark), enter number **10** (text should already be highlighted now), **ctrl+g**.

In this case if the command is not valid, nothing will happen and the text will still be there for you to edit.

## Default keybinds
| Key              | Action                          | Description                                                   |
| ---------------- | ------------------------------- | ------------------------------------------------------------- |
| **Ctrl+D**       | Delete character                | Deletes the character under the cursor (forward delete).      |
| **Ctrl+B**       | Move backward                   | Move cursor one character left.                               |
| **Meta+B**       | Back word                       | Move cursor to the beginning of the previous word.            |
| **Ctrl+F**       | Move forward                    | Move cursor one character right.                              |
| **Meta+F**       | Forward word                    | Move cursor to the end of the next word.                      |
| **Ctrl+L**       | Center cursor                   | Center the current line vertically in the window.             |
| **Ctrl+E**       | End of line                     | Move cursor to the end of the current line.                   |
| **Ctrl+A**       | Start of line                   | Move cursor to the beginning of the current line.             |
| **Ctrl+P**       | Move up                         | Move cursor up one line.                                      |
| **Meta+P**       | Move up (×5)                    | Move cursor up five lines.                                    |
| **Ctrl+N**       | Move down                       | Move cursor down one line.                                    |
| **Meta+N**       | Move down (×5)                  | Move cursor down five lines.                                  |
| **Ctrl+V**       | Page down                       | Scroll down one page.                                         |
| **Meta+V**       | Page up                         | Scroll up one page.                                           |
| **Ctrl+W**       | Write file                      | Save the current buffer to disk.                              |
| **Ctrl+X**       | Exit editor                     | Quit the editor.                                              |
| **Ctrl+H**       | Toggle mark                     | Set or clear the selection mark.                              |
| **Meta+H**       | Toggle mark                     | Same as Ctrl+H (toggle selection mark).                       |
| **Ctrl+G**       | Jump to line                    | Jump to a specific line.                                      |
| **Meta+G**       | Jump to line (keep mark)        | Jump to a specific line while keeping the selection mark.     |
| **Ctrl+Q**       | Find match (forward)            | Search forward for a stored match pattern.                    |
| **Meta+Q**       | Find match (reverse)            | Search backward for the stored match pattern.                 |
| **Meta+Shift+Q** | Store match                     | Save the current search pattern.                              |
| **Ctrl+R**       | Find match (forward, keep mark) | Search forward while keeping the selection mark.              |
| **Meta+R**       | Find match (reverse, keep mark) | Search backward while keeping the selection mark.             |
| **Meta+Shift+R** | Store match (keep mark)         | Store match pattern without clearing mark.                    |
| **Ctrl+O**       | Shell Execute                   | Execute command in forward direction. Default: last command   |
| **Meta+O**       | Shell Execute (preserve)        | Execute command while preserving the input command.           |
| **Meta+Shift+O** | Scheme execute                  | Execute Scheme code.                                          |
| **Ctrl+S**       | New café                        | Enter a **Scheme REPL (`new-cafe`)** inside the editor.       |
| **Ctrl+C**       | Copy selection                  | Copy selected region and clear the mark.                      |
| **Ctrl+K**       | Cut selection                   | Copy and delete selected region.                              |
| **Ctrl+Y**       | Paste                           | Paste previously copied text (replacing selection if active). |
| **Meta+Y**       | Paste                           | Paste without affecting the mark.                             |
| **Ctrl+_**       | Undo                            | Undo last edit.                                               |


## Build Dependencies
* chez scheme

## Build

run the build script
```sh
chez --script build.ss
```

or on some machines chez is just called 'scheme'
```sh
scheme --script build.ss
```

this will produce a binary named 'buged' in the project directory.
