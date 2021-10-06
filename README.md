# ABAP-Emoji

Emoji for ABAP (abapGit)

Made by [Marc Bernard Tools](https://marcbernardtools.com/) giving back to the [SAP Community](https://community.sap.com/)

NO WARRANTIES, [MIT License](LICENSE)

Based on [Twemoji Awesome](http://ellekasai.github.io/twemoji-awesome/) (MIT License)

## Prerequisite

HTML output with Internet connection since Emoji graphics are hosted on https://twemoji.maxcdn.com/.

## Installation

You can install ABAP Emoji using [abapGit](https://github.com/abapGit/abapGit) either creating a new online repository for https://github.com/Marc-Bernard-Tools/ABAP_Emoji or downloading the repository [ZIP file](https://github.com/Marc-Bernard-Tools/ABAP_Emoji/archive/main.zip) and creating a new offline repository.

We recommend to use package `$EMOJI`.

## Usage

### ABAP

```abap
write zcl_emoji=>format_emoji( 'I :heart: ABAP' ).
```

### Output

I ❤️ ABAP

```html
I <i class="twa twa-heart"></i> ABAP
```

Note: Include [`twemoji-awesome.css`](https://github.com/mbtools/ABAP-Emoji/blob/main/css/twemoji-awesome.css) in your HTML output.

## Integrate with abapGit (Developer Version)

![image](img/abapGit_Emoji_Example.png)

1. Insert one line into the following class

```abap
CLASS zcl_abapgit_syntax_highlighter IMPLEMENTATION.
...
  METHOD apply_style.
...
    lv_escaped = show_hidden_chars( lv_escaped ).

    lv_escaped = zcl_emoji=>format_emoji( lv_escaped ). "<<< insert
...
  ENDMETHOD.
```

2. Start transaction `SMW0` > `Binary data` > `$ABAPGIT` 
3. Edit `ZABAPGIT_ICON_FONT_CSS`
4. Append [`twemoji-awesome.css`](https://github.com/mbtools/ABAP-Emoji/blob/main/css/twemoji-awesome.css) to the icon css and save

## Contributions

All contributions are welcome! Just fork this repo and create a pull request. 

## About

<p>Made with :heart: in Canada</p>
<p>Copyright © 2021 <a href="https://marcbernardtools.com/">Marc Bernard Tools</a></p>
<p>Follow <a href="https://twitter.com/marcfbe">@marcfbe</a> on Twitter</p>
<p><a href="https://marcbernardtools.com/"><img width="160" height="65" src="https://marcbernardtools.com/info/MBT_Logo_640x250_on_Gray.png" alt="MBT Logo"></a></p>

