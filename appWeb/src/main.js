let aceEditorInstance;

window.addEventListener('DOMContentLoaded', function() {
    aceEditorInstance = ace.edit("code-input");
    aceEditorInstance.setTheme("ace/theme/textmate");
    aceEditorInstance.session.setMode("ace/mode/text");
    aceEditorInstance.setValue('test', -1); // -1 moves cursor to start
      // Editor behavior - essential QOL features
      aceEditorInstance.setOptions({
        enableBasicAutocompletion: true,
        enableLiveAutocompletion: true,
        showPrintMargin: false,
        fontSize: 14,
        tabSize: 2,
        useSoftTabs: true,
        behavioursEnabled: true, // Enable auto-pairing of brackets
        highlightActiveLine: true,
        wrap: true
    });
    document.getElementById('compile-btn').disabled = false;
});

function handleCompile() {
    // Get code from Ace Editor instance if available
    let code = '';
    if (aceEditorInstance) {
        code = aceEditorInstance.getValue();
    } else {
        code = document.getElementById('code-input').value;
    }
    console.log("Editor value:", code);
    const result = globalCompile(code);
    console.log(result);
    const [error, p4code, transducerDot, p4TransducerDot] = result;
    
    // Clear previous output
    document.getElementById('output').value = '';
    document.getElementById('graph').innerHTML = '';
    
    if (error !== "") {
        // Handle error case
        document.getElementById('output').value = error;
    } else {
        // Success case
        document.getElementById('output').value = p4code;
        
        // Update visualization if we have a transducer dot
        if (transducerDot !== "") {
            Viz.instance().then(function(viz) {
                var svg = viz.renderSVGElement(transducerDot);
                document.getElementById("graph").appendChild(svg);
            });
        }
    }
}
