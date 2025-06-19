function handleCompile() {
    const result = globalCompile(document.getElementById('code-input').value);
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
