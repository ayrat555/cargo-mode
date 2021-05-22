# cargo-mode.el

Emacs minor mode which allows to dynamically select a Cargo command.

Cargo is the Rust package manager.

## Demo

### Simple example (cargo clippy)

![Clippy](demo/demo1.gif)

### Example with custom params (cargo doc --open)

![Doc](demo/demo2.gif)

## Installation

### MELPA

Currently, the package is not available on MELPA.

### From file

Add `cargo-mode.el` to your load path:

``` lisp
(add-to-list 'load-path "path/to/cargo-mode.el")
```

## Setup

Add a hook to the mode that you're using with Rust, for example, `rust-mode`:

``` lisp
(add-hook 'rust-mode-hook 'cargo-minor-mode)
```

Set `compilation-scroll-output` to non-nil to scroll the *cargo-mode* buffer window as output appears. The value ‘first-error’ stops scrolling at the first error, and leaves point on its location in the *cargo-mode* buffer. For example:

``` lisp
(setq compilation-scroll-output t)
```

## Usage

<kbd> C-c d e </kbd> - `cargo-execute-task` - List all available tasks and execute one of them.  As a bonus, you'll get a documentation string because `cargo-mode.el` parses shell output of `cargo --list` directly.

<kbd> C-c d t </kbd> - `cargo-mode-test` - Run all tests in the project (`cargo test`).

<kbd> C-c d l </kbd> - `cargo-mode-last-command` - Execute the last executed command.

<kbd> C-c d b </kbd> - `cargo-mode-build` - Build the project (`cargo build`).


<kbd> C-c d o </kbd> - `cargo-mode-test-current-buffer` - Run all tests in the current buffer.

<kbd> C-c d f </kbd> - `cargo-mode-test-current-test` - Run the current test where pointer is located.


These are all commands that I use most frequently. You can execute any cargo command (fmt, clean etc) available in the project using `cargo-mode-execute-task`. If you have suggestions for additional commands to add keybindings to, please create an issue.

To change prefix (default <kbd>C-c d</kbd>) use:

```el
 (define-key cargo-minor-mode-map (kbd ...) 'cargo-mode-command-map)
```

## Modify a command before execution

Use `C-u` to add extra command line params before executing a command.

## Contributing

1. [Fork it!](https://github.com/ayrat555/cargo-mode/fork)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

## Author

Ayrat Badykov (@ayrat555)
