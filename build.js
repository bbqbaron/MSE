var fs=require('vinyl-fs'),
    cp=require('child_process');

function build() {
    cp.exec('elm-make Ms.elm --output main.js', 
        function (error, stdout, stderr) {
            console.log('beginning compile');
            if (error !== null) {
              console.log('exec error: ' + error);
            } else {
                console.log('cool');
            }
            console.log('compile complete');
    });
}

fs.watch(['./**/*.elm', './*.elm'], build);

build();