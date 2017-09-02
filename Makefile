all:
	stack build
	elm-make --warn client/Main.elm --output=site/scripts/Elm.js
