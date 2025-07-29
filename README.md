# MAB - Managing Annotated Bibliography with Ebib

![Version](https://img.shields.io/static/v1?label=matplotlib-voice-in&message=0.0&color=brightcolor)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Modular and Annotated Bibliography (mab) is an Emacs package that interfaces with  [ebib](https://github.com/joostkremers/ebib) to easily add bibliography entries to your reading lists in org-mode files. 
Ebib provides a GUI interfaces to BibTeX files.
The GUI eases finding and editing BibTeX entries.
It's particularly useful for researchers working with LaTeX and org-mode to maintain annotated bibliographies.

## Features

- Append bibliographic entries from Ebib directly to your reading list org files
- Customizable target file path with interactive selection
- Proper org-mode and LaTeX formatting for bibliography entries
- Creates structured entries with:
  - LaTeX subsection headers using `\bibentry`
  - Automatic TOC entries
  - File includes from your notes directory
  - Organized "Notes" drawer with links to org and PDF files
  - Placeholders for additional content

## Installation

### Manual Installation

1. Download `mab.el` to a directory in your Emacs load path
2. Add the following to your Emacs configuration:

```elisp
(require 'mab)
```

### Using `use-package`

```elisp
(use-package mab
  :load-path "/path/to/mab"
  :after ebib)
```

### Using `straight.el` and `use-package`

```elisp
(use-package mab
  :straight (:host github :repo "MooersLab/mab")
  :after ebib)
```

## Configuration

The package defines a customizable variable:

- `mab-path` - The default path to your reading list org file

You can customize this variable using:

```elisp
(setq mab-path "/path/to/your/mabXXXX.org")
```

Or through the Emacs customization interface:

```
M-x customize-variable RET mab-path RET
```

## Usage

1. Open Ebib and navigate to a bibliography entry you want to add to your mab
2. Press `B` to invoke `mab-add-bib-item`
3. Select the target org file (defaults to the value of `mab-path`)
4. The entry is added to the file under the "Illustrated and annotated bibliography" section

### Example Entry Format

The package adds entries in the following forma for mab files in org-mode format.
The links in the drawer are not exported to PDF.

```org
#+LATEX: \subsection*{\bibentry{AuthorYearTitle}}
#+LATEX: \addcontentsline{toc}{subsection}{AuthorYearTitle}
#+INCLUDE: /Users/blaine/abibNotes/AuthorYearTitle.org
:NOTES:
file:~/abibNotes/AuthorYearTitle.org
file:~/0papersLabeled/AuthorYearTitle.pdf
file:~/0booksLabeled/AuthorYearTitle.pdf
Add more prose. Add tables. Add figures.
:END:
```

## Requirements

- Emacs 25.1 or later
- Ebib 2.0 or later
- org-mode (typically bundled with Emacs)

## File Structure Requirements

This package assumes your reading list org file has a specific structure:
- A section titled `\section*{Illustrated and annotated bibliography}`
- Optionally, a section titled `\section*{Backmatter}` (entries will be inserted before this)
- Referenced files should follow the pattern:
  - Notes: `~/abibNotes/CITEKEY.org`
  - Papers: `~/0papersLabeled/CITEKEY.pdf`
  - Books: `~/0booksLabeled/CITEKEY.pdf`

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `B` | `mab-add-bib-item` | Add the current ebib entry to your mab |

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Author

Blaine Mooers

## Update history

|Version      | Changes                                                                                                                                                                         | Date                 |
|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------|:--------------------|
| Version 0.1 |   Added badges, funding, and update table.  Initial commit.                                                                                                                | 7/28/2025  |

## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)
