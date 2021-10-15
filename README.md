# ABAP Differ

Libraries to highlight the content difference between HTML (or text) strings and between two or three string tables.

Made by [Marc Bernard Tools](https://marcbernardtools.com/) giving back to the [SAP Community](https://community.sap.com/)

NO WARRANTIES, [MIT License](LICENSE)

## HTML Diff

This is a diffing library that understands HTML. Best suited for cases when you want to show a diff of user-generated HTML.

- The implementation is a port of JavaScript (https://github.com/alaorneto/htmldiffer, no license defined)
- which is a port of CoffeeScript (https://github.com/tnwinc/htmldiff.js, MIT license)
- which is a port of the original Ruby (https://github.com/myobie/htmldiff, MIT license)

An enhancement was made so the code can also produce the diff of two texts (tags are treated like text).

## Diff3 Utils

This is a library to find differences between two string tables, generate and apply patches, and perform 3-way merging between an original and two changed string tables. It contains similar functionality to the [GNU Diffutils](https://www.gnu.org/software/diffutils/manual/diffutils.html) tools.

- The implementation is a port of node-diff3 (https://github.com/bhousel/node-diff3, MIT license)

## Prerequisites

SAP Basis 7.4 or higher

## Installation

You can install ABAP Differ using [abapGit](https://github.com/abapGit/abapGit) either creating a new online repository for https://github.com/Marc-Bernard-Tools/ABAP-Differ or downloading the repository [ZIP file](https://github.com/Marc-Bernard-Tools/ABAP-Differ/archive/main.zip) and creating a new offline repository.

We recommend using package `$ABAPDIFF`.

## Usage

### HTML Diff

The following produces the diff of two example HTML snippets:

```abap
DATA:
  lv_original TYPE string,
  lv_modified TYPE string,
  lv_diff     TYPE string,
  lo_differ   TYPE REF TO zcl_abap_differ.

lv_original = '\n'
  && '    <p>First paragraph.</p>\n'
  && '    <ul>\n'
  && '        <li>Item A</li>\n'
  && '        <li>Item B</li>\n'
  && '        <li>Item C</li>\n'
  && '    </ul>\n'
  && '    <img src="previous.jpg">\n'
  && '    <span>This is some interesting text.</span>\n'.

lv_modified = '\n'
  && '    <p>First paragraph.</p>\n'
  && '    <ul>\n'
  && '        <li>Item A</li>\n'
  && '        <li>Item B</li>\n'
  && '        <li>Item D</li>\n'
  && '    </ul>\n'
  && '    <img src="next.jpg">\n'
  && '    <span>This is some new text.</span>\n'.

REPLACE ALL OCCURRENCES OF '\n' IN lv_original WITH cl_abap_char_utilities=>newline.
REPLACE ALL OCCURRENCES OF '\n' IN lv_modified WITH cl_abap_char_utilities=>newline.
  
CREATE OBJECT lo_differ.
  
lv_diff = lo_differ->htmldiff(
  iv_before   = lv_original
  iv_after    = lv_modified
  iv_with_img = abap_false ).
```

Result:

```html
    <p>First paragraph.</p>
    <ul>
        <li>Item A</li>
        <li>Item B</li>
        <li>Item <del>C</del><ins>D</ins></li>
    </ul>
    <img src='previous.jpg'><img src='next.jpg'>
    <span>This is some <del>interesting</del><ins>new</ins> text.</span>
```

By setting the image parameter to true, you can also mark changed images as deletions or insertions:

```abap
lv_diff = lo_differ->htmldiff(
  iv_before   = lv_original
  iv_after    = lv_modified
  iv_with_img = abap_true ).
```  

Result:

```html
    <p>First paragraph.</p>
    <ul>
        <li>Item A</li>
        <li>Item B</li>
        <li>Item <del>C</del><ins>D</ins></li>
    </ul>
    <del><img src='previous.jpg'></del><ins><img src='next.jpg'></ins>
    <span>This is some <del>interesting</del><ins>new</ins> text.</span>
```

There are a few other options you can set in the `constructor`:

- `iv_inserts`: Show `<ins>` tags (default: on)
- `iv_deletes`: Show `<del>` tags (default: on)
- `iv_css_classes`: Add CSS classes to `<ins>` and `<del>` tags (default: off)
- `iv_support_chinese`: Treat Chinese characters as individual words (default: off)

Using CSS classes, the result will distinguish between inserts (class `diffins`), deletes (class `diffdel`), and updates (class `diffmod`).

See the [test classes](https://github.com/Marc-Bernard-Tools/ABAP-Differ/blob/main/src/zcl_htmldiff.clas.testclasses.abap) for more examples.

### Text Diff

todo

### Styling

Here's an examle for styling the insertions and deletions using CSS.

```css
/* CSS for <ins> and <del> tags */
ins { background-color: #ddffdd; }
ins img { border-color: #ddffdd; }

del { background-color: #ffdddd; }
del img { border-color: #ffdddd; }
```

With the CSS class option, use the following:

```css
/* CSS for insert, delete, and modify classes */
.diffins { background-color: #ddffdd; }
.diffdel { background-color: #ffdddd; }
.diffmod { background-color: #ffffdd; }
```

### Diff3

todo

## Contributions

All contributions are welcome! Just fork this repo and create a pull request. 

## About

<p>Made with :heart: in Canada</p>
<p>Copyright © 2021 <a href="https://marcbernardtools.com/">Marc Bernard Tools</a></p>
<p>Follow <a href="https://twitter.com/marcfbe">@marcfbe</a> on Twitter</p>
<p><a href="https://marcbernardtools.com/"><img width="160" height="65" src="https://marcbernardtools.com/info/MBT_Logo_640x250_on_Gray.png" alt="MBT Logo"></a></p>

