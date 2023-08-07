/*
 * Adapted from original elm-gamepad gamepadPort.js, released under the
 * BSD 3-clause license, Copyright (c) 2017, Francesco Orsenigo
 */

export default function(app)
{
  var ElmGamepad = { key : 'sawa-gamepad-mappings',
                     env : { userMappings : localStorage['sawa-gamepad-mappings'] || '',
                             languages : navigator.languages || []
                           },
                     previousFrame : undefined,
                     currentFrame : undefined
                   }

  // Business logic - called every frame
  ElmGamepad.__animationFrame__ = function()
  {
    requestAnimationFrame(ElmGamepad.__animationFrame__);

    ElmGamepad.currentFrame = ElmGamepad.__getFrame__();
    app.ports.onBlob.send([ ElmGamepad.currentFrame, ElmGamepad.previousFrame, ElmGamepad.env ]);
    ElmGamepad.previousFrame = ElmGamepad.currentFrame;
  }

  // Invoked every frame - refresh available gamepads
  ElmGamepad.__getFrame__ = function()
  {
    var mapGamepad = function(gamepad)
    {
      return( {
        axes: gamepad.axes,
        buttons: gamepad.buttons.map(b => [ b.pressed, b.value ]),
        id: gamepad.id,
        index: gamepad.index + 1,
        mapping: gamepad.mapping,
      } );
    }

    var filterGamepad = function(gamepad)
    {
      return(gamepad && gamepad.connected && gamepad.timestamp > 0);
    }

    // Ensure the appropriate browser function for fetching gamepads is known
    // Default to a constant []
    var known = [];
    if(typeof navigator.getGamepads === 'function')
    {
      known = navigator.getGamepads().filter(filterGamepad)
    }
    else if(typeof navigator.webkitGetGamepads === 'function')
    {
      known = navigator.webkitGetGamepads().filter(filterGamepad)
    }
    return({ gamepads : known.map(mapGamepad), timestamp : Date.now() });
  }

  //
  // User-invoked function
  //

  // save : String -> Cmd a
  ElmGamepad.save = function(mappings)
  {
    localStorage.setItem(ElmGamepad.key, mappings);
  }

  // load : () -> Cmd a
  ElmGamepad.load = function(_)
  {
    if(localStorage.length > 0 && localStorage[ElmGamepad.key] != undefined)
    {
      app.ports.onLoad.send(localStorage[ElmGamepad.key]);
    }
  }

  app.ports.loadMappings.subscribe(ElmGamepad.load);
  app.ports.saveMappings.subscribe(ElmGamepad.save);
  ElmGamepad.previousFrame = ElmGamepad.__getFrame__();
  requestAnimationFrame(ElmGamepad.__animationFrame__);
}
