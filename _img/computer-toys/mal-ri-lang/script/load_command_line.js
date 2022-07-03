var editor = ace.edit("editor");

editor.setOptions({
  // editor options
  // selectionStyle: "line"|"text"
  // highlightActiveLine: true|false
  // highlightSelectedWord: true|false
  // readOnly: true|false
  // cursorStyle: "ace"|"slim"|"smooth"|"wide"
  // mergeUndoDeltas: false|true|"always"
  // behavioursEnabled: boolean
  // wrapBehavioursEnabled: boolean
    // this is needed if editor is inside scrollable page
  // autoScrollEditorIntoView: boolean (defaults to false)
    // copy/cut the full line if selection is empty, defaults to false
  // copyWithEmptySelection: boolean 
  // enableMultiselect: boolean   # on by default
  useSoftTabs: true,
  navigateWithinSoftTabs: true,

  tabSize: 2,

  wrap: true,
  maxLines: 10,

  showGutter: false,
  showLineNumbers: true,
});

/*
editor.commands.addCommand({
  name: 'evaluateCode',
  bindKey: {
    win: 'Shift-Enter',
    mac: 'Shift-Enter',
  },
  exec: function(editor) {
    let code = editor.getValue();
    editor.setValue("");
    try {
      console.log(window.rilang.$re(code, function (res, exp) {
        alert(`${res}, ${exp}`);
      }));
    } catch (error) {
      alert(error)
    }
  }
})
*/

editor.commands.addCommand({
  name: 'evaluateCode',
  bindKey: {
    win: 'Shift-Enter',
    mac: 'Shift-Enter',
  },
  exec: function(editor) {
    let code = editor.getValue();
    editor.setValue("");
    try {
      console.log(window.rilang.$re(code, function (res, exp) {
        let history = document.getElementById("history");
        let res_str = typeof res == 'function' ? 'function ...' : res
        history.innerHTML += `
        <div class="history_card">
          <div>${exp}</div>
          <div>;; => ${res_str}</div>
        </div>
        `
      }));
    } catch (error) {
      alert(error)
    }
  }
})

window.onload = function () {
  window.aceEditor = editor;
}