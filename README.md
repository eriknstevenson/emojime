#EmojiMe

Get Emoji placeholder SVG on demand.

##Building

```
git clone https://github.com/narrative/emojime
cd emojime
stack build && stack exec emojime-exe
curl localhost:3000/250/250/red
```


##Usage

| Method | Endpoint | Description
|---|---|---|
| GET | /{width}/{height} | Request a random emoji of given size
| GET | /{width}/{height}/{color}| Returns a random emoji of given size with given background color

##Planned features

- Request specific emoji (By unicode, or by shortname)
- Random background colors
- Disable/Enable the size label on returned SVG.

##Credits

Emoji art provided courtesy of [EmojiOne](http://emojione.com/). Thank you!

