import { Elm } from './src/Main.elm';
import {StreamLanguage} from "@codemirror/stream-parser";
import {indentWithTab} from "@codemirror/commands";
import {keymap} from "@codemirror/view";
import {EditorState, EditorView, basicSetup} from "@codemirror/basic-setup";
import {clojure} from "@codemirror/legacy-modes/mode/clojure";


const STORAGE_KEY = "data"

customElements.define("code-editor",
    class extends HTMLElement {
        constructor() {
            super();

            this._editorValue = ""
        }

        connectedCallback() {
            this._editor = new EditorView({
                state: EditorState.create({
                    extensions: [
                        basicSetup,
                        keymap.of([indentWithTab]),
                        StreamLanguage.define(clojure),
                        EditorView.updateListener.of(update => {
                            if (!update.docChanged) return;
                            const value = update.view.state.doc.toString();
                            if (this._editorValue === value) return;
                            this._editorValue = value;
                            this.dispatchEvent(new CustomEvent("editorChanged"));
                        })
                    ],
                    doc: this._editorValue
                }),
                parent: this,
            })
        }

        get editorValue () {
            return this._editorValue;
        }

        set editorValue(value) {
            if (!this._editorValue === value) return;
            this._editorValue = value;
            if (!this._editor) return;
            const state = this._editor.state;
            const transaction = state.update({
                changes: {from: 0, to: state.doc.length, insert: value},
                selection: state.selection, // keep the selection the same, as we actually didn't change anything
            });
            this._editor.dispatch(transaction);
        }

    }
);

const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: {
        input: window.localStorage.getItem(STORAGE_KEY)
    }
});


app.ports.save.subscribe((msg) => {
    window.localStorage.setItem(STORAGE_KEY, msg);
});