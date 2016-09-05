#EmojiMe![1f61c](https://cdnjs.cloudflare.com/ajax/libs/emojione/2.2.6/assets/png/1f61c.png)

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
| GET | /{width}/{height} | Returns a random emoji of given size.
| GET | /{width}/{height}/{color}| Returns a random emoji of given size with given background color.

##Planned features

- [ ] Request specific emoji (By unicode, or by shortname).
- [ ] Random background colors.
- [ ] Disable/Enable the size label on returned SVG.
- [ ] Homepage

##Credits

Emoji art provided courtesy of [EmojiOne](http://emojione.com/). Thank you!

