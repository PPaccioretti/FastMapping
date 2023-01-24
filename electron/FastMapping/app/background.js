/******/ (() => { // webpackBootstrap
/******/ 	"use strict";
/******/ 	var __webpack_modules__ = ({

/***/ "./src/background.js":
/*!***************************!*\
  !*** ./src/background.js ***!
  \***************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony import */ var fs_jetpack__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! fs-jetpack */ "fs-jetpack");
/* harmony import */ var fs_jetpack__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(fs_jetpack__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var execa__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! execa */ "execa");
/* harmony import */ var execa__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(execa__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var path__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! path */ "path");
/* harmony import */ var path__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(path__WEBPACK_IMPORTED_MODULE_2__);
/*Some parts of this have been rewritten and/or replaced with 
code from https://github.com/dirkschumacher/r-shiny-electron
MIT License

Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.*/

// Modules to control application life and create native browser window
const {
  app,
  session,
  BrowserWindow,
  Menu
} = __webpack_require__(/*! electron */ "electron");



const MACOS = "darwin";
const WINDOWS = "win32";
const fs = __webpack_require__(/*! fs */ "fs");
const backgroundColor = "#2c3e50";
const waitFor = milliseconds => {
  return new Promise((resolve, _reject) => {
    setTimeout(resolve, milliseconds);
  });
};
// Log to:
//Linux: ~/.config/<app name>/log.log
//macOS: ~/Library/Logs/<app name>/log.log
//Windows: %USERPROFILE%\AppData\Roaming\<app name>\log.log
const log = __webpack_require__(/*! electron-log */ "electron-log");
log.info("Application Started");

//Find and bind an open port
//Assigned port can be accesssed with srv.address().port
var net = __webpack_require__(/*! net */ "net");
var srv = net.createServer(function (sock) {
  sock.end("Hello world\n");
});
srv.listen(0, function () {
  console.log("Listening on port " + srv.address().port);
});
let NODER = null;
// folder above "bin/RScript"
if (process.platform == WINDOWS) {
  var rResources = path__WEBPACK_IMPORTED_MODULE_2___default().join(app.getAppPath(), "app", "r_lang");
  //Unfortunately on MacOS paths are hardcoded into
  //Rscript but it's in binary so have to use R instead
  NODER = path__WEBPACK_IMPORTED_MODULE_2___default().join(rResources, "bin", "x64", "Rterm.exe");
}
if (process.platform == MACOS) {
  var rVer = fs.readdirSync(path__WEBPACK_IMPORTED_MODULE_2___default().join(app.getAppPath(), "app", "r_lang", "Library", "Frameworks", "R.framework", "Versions")).filter(fn => fn.match(/\d+\.(?:\d+|x)(?:\.\d+|x){0,1}/g));
  var rResources = path__WEBPACK_IMPORTED_MODULE_2___default().join(app.getAppPath(), "app", "r_lang", "Library", "Frameworks", "R.framework", "Versions", rVer.toString(), "Resources");
  //Unfortunately on MacOS paths are hardcoded into
  //Rscript but it's in binary so have to use R instead

  NODER = path__WEBPACK_IMPORTED_MODULE_2___default().join(rResources, "bin", "R");
}
let rShinyProcess = null;
let shutdown = false;
// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow;
let loadingSplashScreen;
let errorSplashScreen;
const createWindow = shinyUrl => {
  mainWindow = new BrowserWindow({
    width: 1200,
    height: 750,
    show: false,
    icon: "FastMapping.ico",
    fullscreen: false,
    frame: true,
    titleBarStyle: "hidden",
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true
    }
  });
  mainWindow.on("closed", () => {
    mainWindow = null;
  });
};

// tries to start a webserver
// attempt - a counter how often it was attempted to start a webserver
// use the progress call back to listen for intermediate status reports
// use the onErrorStartup callback to react to a critical failure during startup
// use the onErrorLater callback to handle the case when the R process dies
// use onSuccess to retrieve the shinyUrl
const tryStartWebserver = async (attempt, progressCallback, onErrorStartup, onErrorLater, onSuccess) => {
  if (attempt > 3) {
    await progressCallback({
      attempt: attempt,
      code: "failed"
    });
    await onErrorStartup();
    return;
  }
  if (rShinyProcess !== null) {
    await onErrorStartup(); // should not happen
    return;
  }
  await progressCallback({
    attempt: attempt,
    code: "start"
  });
  let shinyRunning = false;
  const onError = async e => {
    console.error(e);
    rShinyProcess = null;
    if (shutdown) {
      // global state :(
      return;
    }
    if (shinyRunning) {
      await onErrorLater();
    } else {
      await tryStartWebserver(attempt + 1, progressCallback, onErrorStartup, onErrorLater, onSuccess);
    }
  };
  let shinyProcessAlreadyDead = false;
  rShinyProcess = execa__WEBPACK_IMPORTED_MODULE_1___default()(NODER, ["--vanilla", "-e", "FastMapping::run_app(options = list(port = " + srv.address().port + "))"], {
    env: {
      //Necessary for letting R know where it is and ensure we're not using another R
      WITHIN_ELECTRON: "T",
      // can be used within an app to implement specific behaviour
      RHOME: rResources,
      R_HOME_DIR: rResources,
      R_LIBS: path__WEBPACK_IMPORTED_MODULE_2___default().join(rResources, "library"),
      R_LIBS_USER: path__WEBPACK_IMPORTED_MODULE_2___default().join(rResources, "library"),
      R_LIBS_SITE: path__WEBPACK_IMPORTED_MODULE_2___default().join(rResources, "library"),
      R_LIB_PATHS: path__WEBPACK_IMPORTED_MODULE_2___default().join(rResources, "library")
    }
  }).catch(e => {
    shinyProcessAlreadyDead = true;
    onError(e);
  });
  for (let i = 0; i <= 10; i++) {
    if (shinyProcessAlreadyDead) {
      break;
    }
    try {
      if (shinyRunning === false) {
        mainWindow.loadURL("http://127.0.0.1:" + srv.address().port);
        await waitFor(i * 1000);
        mainWindow.webContents.executeJavaScript("window.Shiny.shinyapp.isConnected()", true).then(result => {
          shinyRunning = true;
          mainWindow.show();
          loadingSplashScreen.close();
          console.log("Trying to connect to Shiny... connected");
        }).catch(result => {
          if (shinyRunning === false) {
            console.log("Trying to connect to Shiny... " + i);
          }
        });
      }
    } catch (e) {}
  }
  await progressCallback({
    attempt: attempt,
    code: "notresponding"
  });
  try {
    rShinyProcess.kill();
  } catch (e) {}
};
const splashScreenOptions = {
  width: 500,
  height: 200,
  frame: false,
  icon: "FastMapping.ico",
  backgroundColor: backgroundColor
};
const createSplashScreen = filename => {
  let splashScreen = new BrowserWindow(splashScreenOptions);
  splashScreen.loadURL(path__WEBPACK_IMPORTED_MODULE_2___default().join(app.getAppPath(), "app", filename + ".html"));
  splashScreen.on("closed", () => {
    splashScreen = null;
  });
  return splashScreen;
};
const createLoadingSplashScreen = () => {
  loadingSplashScreen = createSplashScreen("loading");
};
const createErrorScreen = () => {
  errorSplashScreen = createSplashScreen("failed");
};

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on("ready", async () => {
  createWindow();
  // Menu
  // Set null Menu
  Menu.setApplicationMenu(null);

  // Set a content security policy
  session.defaultSession.webRequest.onHeadersReceived((_, callback) => {
    callback({
      responseHeaders: `
        default-src 'none';
        script-src 'self';
        img-src 'self' data:;
        style-src 'self';
        font-src 'self';
      `
    });
  });

  // Deny all permission requests
  /*  session.defaultSession.setPermissionRequestHandler((_1, _2, callback) => {
      callback(false)
    })
  */
  createLoadingSplashScreen();
  const emitSpashEvent = async (event, data) => {
    try {
      await loadingSplashScreen.webContents.send(event, data);
    } catch (e) {}
  };

  // pass the loading events down to the loadingSplashScreen window
  const progressCallback = async event => {
    await emitSpashEvent("start-webserver-event", event);
  };
  const onErrorLater = async () => {
    if (!mainWindow) {
      // fired when we quit the app
      return;
    }
    createErrorScreen();
    await errorSplashScreen.show();
    mainWindow.destroy();
  };
  const onErrorStartup = async () => {
    await waitFor(1000); // TODO: hack, only emit if the loading screen is ready
    await emitSpashEvent("failed");
  };
  await tryStartWebserver(0, progressCallback, onErrorStartup, onErrorLater, url => {});
});

// Quit when all windows are closed.
app.on("window-all-closed", () => {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  // if (process.platform !== 'darwin') {
  // }
  // We overwrite the behaviour for now as it makes things easier
  // remove all events
  shutdown = true;
  app.quit();

  // kill the process, just in case
  // usually happens automatically if the main process is killed
  try {
    rShinyProcess.kill();
  } catch (e) {}
});
app.on("activate", () => {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  //if (mainWindow === null) {
  //  createWindow()
  //}
  // Deactivated for now
});

/***/ }),

/***/ "electron":
/*!***************************!*\
  !*** external "electron" ***!
  \***************************/
/***/ ((module) => {

module.exports = require("electron");;

/***/ }),

/***/ "electron-log":
/*!*******************************!*\
  !*** external "electron-log" ***!
  \*******************************/
/***/ ((module) => {

module.exports = require("electron-log");;

/***/ }),

/***/ "execa":
/*!************************!*\
  !*** external "execa" ***!
  \************************/
/***/ ((module) => {

module.exports = require("execa");;

/***/ }),

/***/ "fs":
/*!*********************!*\
  !*** external "fs" ***!
  \*********************/
/***/ ((module) => {

module.exports = require("fs");;

/***/ }),

/***/ "fs-jetpack":
/*!*****************************!*\
  !*** external "fs-jetpack" ***!
  \*****************************/
/***/ ((module) => {

module.exports = require("fs-jetpack");;

/***/ }),

/***/ "net":
/*!**********************!*\
  !*** external "net" ***!
  \**********************/
/***/ ((module) => {

module.exports = require("net");;

/***/ }),

/***/ "path":
/*!***********************!*\
  !*** external "path" ***!
  \***********************/
/***/ ((module) => {

module.exports = require("path");;

/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		if(__webpack_module_cache__[moduleId]) {
/******/ 			return __webpack_module_cache__[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/compat get default export */
/******/ 	(() => {
/******/ 		// getDefaultExport function for compatibility with non-harmony modules
/******/ 		__webpack_require__.n = (module) => {
/******/ 			var getter = module && module.__esModule ?
/******/ 				() => module['default'] :
/******/ 				() => module;
/******/ 			__webpack_require__.d(getter, { a: getter });
/******/ 			return getter;
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/define property getters */
/******/ 	(() => {
/******/ 		// define getter functions for harmony exports
/******/ 		__webpack_require__.d = (exports, definition) => {
/******/ 			for(var key in definition) {
/******/ 				if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
/******/ 					Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
/******/ 				}
/******/ 			}
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/hasOwnProperty shorthand */
/******/ 	(() => {
/******/ 		__webpack_require__.o = (obj, prop) => Object.prototype.hasOwnProperty.call(obj, prop)
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/make namespace object */
/******/ 	(() => {
/******/ 		// define __esModule on exports
/******/ 		__webpack_require__.r = (exports) => {
/******/ 			if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 				Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 			}
/******/ 			Object.defineProperty(exports, '__esModule', { value: true });
/******/ 		};
/******/ 	})();
/******/ 	
/************************************************************************/
/******/ 	// startup
/******/ 	// Load entry module
/******/ 	__webpack_require__("./src/background.js");
/******/ 	// This entry module used 'exports' so it can't be inlined
/******/ })()
;
//# sourceMappingURL=background.js.map