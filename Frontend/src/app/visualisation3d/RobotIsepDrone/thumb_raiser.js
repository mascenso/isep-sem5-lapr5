// Thumb Raiser - JPP 2021, 2022, 2023
// 3D modeling
// 3D models importing
// Perspective and orthographic projections
// Viewing
// Linear and affine transformations
// Lighting and materials
// Shadow projection
// Fog
// Texture mapping
// User interaction

import * as THREE from "three";
import Stats from "three/addons/libs/stats.module.js";
import Orientation from "./orientation.js";
import { generalData, mazeData, playerData, lightsData, fogData, cameraData, cubeTextureData } from "./default_data.js";
import { merge } from "./merge.js";
import Maze from "./maze.js";
import Player from "./player.js";
import Lights from "./lights.js";
import Fog from "./fog.js";
import Camera from "./camera.js";
import Animations from "./animations.js";
import CubeTexture from "./cubetexture.js";

import UserInterface from "./user_interface.js";

/*
 * generalParameters = {
 *  setDevicePixelRatio: Boolean
 * }
 *
 * mazeParameters = {
 *  url: String,
 *  credits: String,
 *  scale: Vector3
 * }
 *
 *  * cubeTexturesParameters = {
 *  skyboxes: [{
 *   name: String,
 *   texturePath: String,
 *   texturePositiveXUrl: String,
 *   textureNegativeXUrl: String,
 *   texturePositiveYUrl: String,
 *   textureNegativeYUrl: String,
 *   texturePositiveZUrl: String,
 *   textureNegativeZUrl: String,
 *   credits: String
 *  }],
 *  selected: Integer
 * }
 *
 * playerParameters = {
 *  url: String,
 *  credits: String,
 *  scale: Vector3,
 *  walkingSpeed: Float,
 *  initialDirection: Float,
 *  turningSpeed: Float,
 *  runningFactor: Float,
 *  keyCodes: { fixedView: String, firstPersonView: String, thirdPersonView: String, topView: String, viewMode: String, userInterface: String, miniMap: String, help: String, statistics: String, run: String, left: String, right: String, backward: String, forward: String, jump: String, yes: String, no: String, wave: String, punch: String, thumbsUp: String }
 * }
 *
 * lightsParameters = {
 *  ambientLight: { color: Integer, intensity: Float },
 *  pointLight1: { color: Integer, intensity: Float, range: Float, position: Vector3 },
 *  pointLight2: { color: Integer, intensity: Float, range: Float, position: Vector3 },
 *  spotLight: { color: Integer, intensity: Float, range: Float, angle: Float, penumbra: Float, position: Vector3, direction: Float }
 * }
 *
 * fogParameters = {
 *  enabled: Boolean,
 *  color: Integer,
 *  near: Float,
 *  far: Float
 * }
 *
 * fixedViewCameraParameters = {
 *  view: String,
 *  multipleViewsViewport: Vector4,
 *  target: Vector3,
 *  initialOrientation: Orientation,
 *  orientationMin: Orientation,
 *  orientationMax: Orientation,
 *  initialDistance: Float,
 *  distanceMin: Float,
 *  distanceMax: Float,
 *  initialZoom: Float,
 *  zoomMin: Float,
 *  zoomMax: Float,
 *  initialFov: Float,
 *  near: Float,
 *  far: Float
 * }
 *
 * firstPersonViewCameraParameters = {
 *  view: String,
 *  multipleViewsViewport: Vector4,
 *  target: Vector3,
 *  initialOrientation: Orientation,
 *  orientationMin: Orientation,
 *  orientationMax: Orientation,
 *  initialDistance: Float,
 *  distanceMin: Float,
 *  distanceMax: Float,
 *  initialZoom: Float,
 *  zoomMin: Float,
 *  zoomMax: Float,
 *  initialFov: Float,
 *  near: Float,
 *  far: Float
 * }
 *
 * thirdPersonViewCameraParameters = {
 *  view: String,
 *  multipleViewsViewport: Vector4,
 *  target: Vector3,
 *  initialOrientation: Orientation,
 *  orientationMin: Orientation,
 *  orientationMax: Orientation,
 *  initialDistance: Float,
 *  distanceMin: Float,
 *  distanceMax: Float,
 *  initialZoom: Float,
 *  zoomMin: Float,
 *  zoomMax: Float,
 *  initialFov: Float,
 *  near: Float,
 *  far: Float
 * }
 *
 * topViewCameraParameters = {
 *  view: String,
 *  multipleViewsViewport: Vector4,
 *  target: Vector3,
 *  initialOrientation: Orientation,
 *  orientationMin: Orientation,
 *  orientationMax: Orientation,
 *  initialDistance: Float,
 *  distanceMin: Float,
 *  distanceMax: Float,
 *  initialZoom: Float,
 *  zoomMin: Float,
 *  zoomMax: Float,
 *  initialFov: Float,
 *  near: Float,
 *  far: Float
 * }
 *
 * miniMapCameraParameters = {
 *  view: String,
 *  multipleViewsViewport: Vector4,
 *  initialOrientation: Orientation,
 *  orientationMin: Orientation,
 *  orientationMax: Orientation,
 *  initialDistance: Float,
 *  distanceMin: Float,
 *  distanceMax: Float,
 *  initialZoom: Float,
 *  zoomMin: Float,
 *  zoomMax: Float,
 *  initialFov: Float,
 *  near: Float,
 *  far: Float
 * }
 */

export default class ThumbRaiser {
    constructor(generalParameters, mazeParameters, playerParameters, lightsParameters, fogParameters, fixedViewCameraParameters, firstPersonViewCameraParameters, thirdPersonViewCameraParameters, topViewCameraParameters, miniMapCameraParameters, cubeTexturesParameters, buildingService, floorService) {
        this.generalParameters = merge({}, generalData, generalParameters);
        this.mazeParameters = merge({}, mazeData, mazeParameters);
        this.playerParameters = merge({}, playerData, playerParameters);
        this.lightsParameters = merge({}, lightsData, lightsParameters);
        this.fogParameters = merge({}, fogData, fogParameters);
        this.fixedViewCameraParameters = merge({}, cameraData, fixedViewCameraParameters);
        this.firstPersonViewCameraParameters = merge({}, cameraData, firstPersonViewCameraParameters);
        this.thirdPersonViewCameraParameters = merge({}, cameraData, thirdPersonViewCameraParameters);
        this.topViewCameraParameters = merge({}, cameraData, topViewCameraParameters);
        this.miniMapCameraParameters = merge({}, cameraData, miniMapCameraParameters);
        this.cubeTexturesParameters = merge({}, cubeTextureData, cubeTexturesParameters);

        // Create a 2D scene (the viewports frames)
        this.scene2D = new THREE.Scene();

        // Create a square
        let points = [new THREE.Vector3(0.0, 0.0, 0.0), new THREE.Vector3(1.0, 0.0, 0.0), new THREE.Vector3(1.0, 1.0, 0.0), new THREE.Vector3(0.0, 1.0, 0.0)];
        let geometry = new THREE.BufferGeometry().setFromPoints(points);
        const material = new THREE.LineBasicMaterial({ color: 0xffffff });
        this.square = new THREE.LineLoop(geometry, material);
        this.scene2D.add(this.square);

        // Create the camera corresponding to the 2D scene
        this.camera2D = new THREE.OrthographicCamera(0.0, 1.0, 1.0, 0.0, 0.0, 1.0);

        // Create a 3D scene (the game itself)
        this.scene3D = new THREE.Scene();


        // Create the cube texture
        this.cubeTexture = new CubeTexture(this.cubeTexturesParameters.skyboxes[0]);

        // Add background
        this.scene3D.background = this.cubeTexture.textures;

        // Create the maze
        this.maze = new Maze(this.mazeParameters);

        // Create the player
        this.player = new Player(this.playerParameters);

        // Create the lights
        this.lights = new Lights(this.lightsParameters);

        // Create the fog
        this.fog = new Fog(this.fogParameters);

        // Create the cameras corresponding to the four different views: fixed view, first-person view, third-person view and top view
        this.fixedViewCamera = new Camera(this.fixedViewCameraParameters, window.innerWidth, window.innerHeight);
        this.firstPersonViewCamera = new Camera(this.firstPersonViewCameraParameters, window.innerWidth, window.innerHeight);
        this.thirdPersonViewCamera = new Camera(this.thirdPersonViewCameraParameters, window.innerWidth, window.innerHeight);
        this.topViewCamera = new Camera(this.topViewCameraParameters, window.innerWidth, window.innerHeight);

        // Create the mini-map camera
        this.miniMapCamera = new Camera(this.miniMapCameraParameters, window.innerWidth, window.innerHeight);

        // Create the statistics and make its node invisible
        this.statistics = new Stats();
        this.statistics.dom.style.visibility = "hidden";
        document.body.appendChild(this.statistics.dom);

        // Create a renderer and turn on shadows in the renderer
        let canvas = document.getElementById("canvasForRender");
        this.renderer = new THREE.WebGLRenderer({ canvas: canvas });
        if (this.generalParameters.setDevicePixelRatio) {
            this.renderer.setPixelRatio(window.devicePixelRatio);
        }
        this.renderer.autoClear = false;
        this.renderer.shadowMap.enabled = true;
        this.renderer.shadowMap.type = THREE.PCFSoftShadowMap;
        const height = window.innerHeight - 50;
        const width = window.innerWidth - 50;
        this.renderer.setSize(width, height);

        // Set the mouse move action (none)
        this.dragMiniMap = false;
        this.changeCameraDistance = false;
        this.changeCameraOrientation = false;

        // Set the game state
        this.gameRunning = false;

        // Get and configure the panel's <div> elements
        this.viewsPanel = document.getElementById("views-panel");
        this.view = document.getElementById("view");
        this.projection = document.getElementById("projection");
        this.horizontal = document.getElementById("horizontal");
        this.horizontal.step = 1;
        this.vertical = document.getElementById("vertical");
        this.vertical.step = 1;
        this.distance = document.getElementById("distance");
        this.distance.step = 0.1;
        this.zoom = document.getElementById("zoom");
        this.zoom.step = 0.1;
        this.reset = document.getElementById("reset");
        this.resetAll = document.getElementById("reset-all");
        this.helpPanel = document.getElementById("help-panel");
        this.helpPanel.style.visibility = "hidden";
        this.subwindowsPanel = document.getElementById("subwindows-panel");
        this.multipleViewsCheckBox = document.getElementById("multiple-views");
        this.multipleViewsCheckBox.checked = false;
        this.userInterfaceCheckBox = document.getElementById("user-interface");
        this.userInterfaceCheckBox.checked = true;
        this.miniMapCheckBox = document.getElementById("mini-map");
        this.miniMapCheckBox.checked = true;
        this.helpCheckBox = document.getElementById("help");
        this.helpCheckBox.checked = false;
        this.statisticsCheckBox = document.getElementById("statistics");
        this.statisticsCheckBox.checked = false;

        // Build the help panel
        this.buildHelpPanel();

        // Set the active view camera (fixed view)
        this.setActiveViewCamera(this.fixedViewCamera);

        // Arrange viewports by view mode
        this.arrangeViewports(this.multipleViewsCheckBox.checked);

        // Register the event handler to be called on window resize
        window.addEventListener("resize", event => this.windowResize(event));

        // Register the event handler to be called on key down
        document.addEventListener("keydown", event => this.keyChange(event, true));

        // Register the event handler to be called on key release
        document.addEventListener("keyup", event => this.keyChange(event, false));

        // Register the event handler to be called on mouse down
        this.renderer.domElement.addEventListener("mousedown", event => this.mouseDown(event));

        // Register the event handler to be called on mouse move
        this.renderer.domElement.addEventListener("mousemove", event => this.mouseMove(event));

        // Register the event handler to be called on mouse up
        this.renderer.domElement.addEventListener("mouseup", event => this.mouseUp(event));

        // Register the event handler to be called on mouse wheel
        this.renderer.domElement.addEventListener("wheel", event => this.mouseWheel(event));

        // Register the event handler to be called on context menu
        this.renderer.domElement.addEventListener("contextmenu", event => this.contextMenu(event));

        // Register the event handler to be called on select, input number, or input checkbox change
        this.view.addEventListener("change", event => this.elementChange(event));
        this.projection.addEventListener("change", event => this.elementChange(event));
        this.horizontal.addEventListener("change", event => this.elementChange(event));
        this.vertical.addEventListener("change", event => this.elementChange(event));
        this.distance.addEventListener("change", event => this.elementChange(event));
        this.zoom.addEventListener("change", event => this.elementChange(event));
        this.multipleViewsCheckBox.addEventListener("change", event => this.elementChange(event));
        this.userInterfaceCheckBox.addEventListener("change", event => this.elementChange(event));
        this.helpCheckBox.addEventListener("change", event => this.elementChange(event));
        this.statisticsCheckBox.addEventListener("change", event => this.elementChange(event));

        // Register the event handler to be called on input button click
        this.reset.addEventListener("click", event => this.buttonClick(event));
        this.resetAll.addEventListener("click", event => this.buttonClick(event));

        this.activeElement = document.activeElement;

        this.listFloors = [];
        this.floorActual = {};

        //injeção dos serviços (não é a melhor pratica)
        this.buildingService = buildingService;
        this.floorService = floorService;

        // para guardar a informação da passagem referente ao proximo mapa a carregar
        this.bridgeInfo = "";

        //modo automatico
        this.automaticMode = false;

    }

    buildHelpPanel() {
        const table = document.getElementById("help-table");
        let i = 0;
        for (const key in this.player.keyCodes) {
            while (table.rows[i].cells.length < 2) {
                i++;
            };
            table.rows[i++].cells[0].innerHTML = this.player.keyCodes[key];
        }
        table.rows[i].cells[0].innerHTML = this.maze.credits + "<br>" + this.player.credits;
    }

    displayPanel() {
        this.view.options.selectedIndex = ["fixed", "first-person", "third-person", "top"].indexOf(this.activeViewCamera.view);
        this.projection.options.selectedIndex = ["perspective", "orthographic"].indexOf(this.activeViewCamera.projection);
        this.horizontal.value = this.activeViewCamera.orientation.h.toFixed(0);
        this.vertical.value = this.activeViewCamera.orientation.v.toFixed(0);
        this.distance.value = this.activeViewCamera.distance.toFixed(1);
        this.zoom.value = this.activeViewCamera.zoom.toFixed(1);
    }

    // Set active view camera
    setActiveViewCamera(camera) {
        this.activeViewCamera = camera;
        this.horizontal.min = this.activeViewCamera.orientationMin.h.toFixed(0);
        this.horizontal.max = this.activeViewCamera.orientationMax.h.toFixed(0);
        this.vertical.min = this.activeViewCamera.orientationMin.v.toFixed(0);
        this.vertical.max = this.activeViewCamera.orientationMax.v.toFixed(0);
        this.distance.min = this.activeViewCamera.distanceMin.toFixed(1);
        this.distance.max = this.activeViewCamera.distanceMax.toFixed(1);
        this.zoom.min = this.activeViewCamera.zoomMin.toFixed(1);
        this.zoom.max = this.activeViewCamera.zoomMax.toFixed(1);
        this.displayPanel();
    }

    arrangeViewports(multipleViews) {
        this.fixedViewCamera.setViewport(multipleViews);
        this.firstPersonViewCamera.setViewport(multipleViews);
        this.thirdPersonViewCamera.setViewport(multipleViews);
        this.topViewCamera.setViewport(multipleViews);
    }

    pointerIsOverViewport(pointer, viewport) {
        return (
            pointer.x >= viewport.x &&
            pointer.x < viewport.x + viewport.width &&
            pointer.y >= viewport.y &&
            pointer.y < viewport.y + viewport.height);
    }

    getPointedViewport(pointer) {
        let viewport;
        // Check if the pointer is over the mini-map camera viewport
        if (this.miniMapCheckBox.checked) {
            viewport = this.miniMapCamera.getViewport();
            if (this.pointerIsOverViewport(pointer, viewport)) {
                return this.miniMapCamera.view;
            }
        }
        // Check if the pointer is over the remaining camera viewports
        let cameras;
        if (this.multipleViewsCheckBox.checked) {
            cameras = [this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera];
        }
        else {
            cameras = [this.activeViewCamera];
        }
        for (const camera of cameras) {
            viewport = camera.getViewport();
            if (this.pointerIsOverViewport(pointer, viewport)) {
                return camera.view;
            }
        }
        // No camera viewport is being pointed
        return "none";
    }

    setViewMode(multipleViews) { // Single-view mode: false; multiple-views mode: true
        this.multipleViewsCheckBox.checked = multipleViews;
        this.arrangeViewports(this.multipleViewsCheckBox.checked);
    }

    setUserInterfaceVisibility(visible) {
        this.userInterfaceCheckBox.checked = visible;
        this.viewsPanel.style.visibility = visible ? "visible" : "hidden";
        this.subwindowsPanel.style.visibility = visible ? "visible" : "hidden";
        this.userInterface.setVisibility(visible);
    }

    setMiniMapVisibility(visible) { // Hidden: false; visible: true
        this.miniMapCheckBox.checked = visible;
    }

    setHelpVisibility(visible) { // Hidden: false; visible: true
        this.helpCheckBox.checked = visible;
        this.helpPanel.style.visibility = visible ? "visible" : "hidden";
    }

    setStatisticsVisibility(visible) { // Hidden: false; visible: true
        this.statisticsCheckBox.checked = visible;
        this.statistics.dom.style.visibility = visible ? "visible" : "hidden";
    }

    windowResize() {
        this.fixedViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.firstPersonViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.thirdPersonViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.topViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.miniMapCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.renderer.setSize(window.innerWidth, window.innerHeight);
    }

    keyChange(event, state) {
        // Allow digit and arrow keys to be used when entering numbers
        if (["horizontal", "vertical", "distance", "zoom"].indexOf(event.target.id) < 0) {
            event.target.blur();
        }
        if (document.activeElement == document.body) {
            // Prevent the "Space" and "Arrow" keys from scrolling the document's content
            if (event.code == "Space" || event.code == "ArrowLeft" || event.code == "ArrowRight" || event.code == "ArrowDown" || event.code == "ArrowUp") {
                event.preventDefault();
            }
            if (event.code == this.player.keyCodes.fixedView && state) { // Select fixed view
                this.setActiveViewCamera(this.fixedViewCamera);
            }
            else if (event.code == this.player.keyCodes.firstPersonView && state) { // Select first-person view
                this.setActiveViewCamera(this.firstPersonViewCamera);
            }
            else if (event.code == this.player.keyCodes.thirdPersonView && state) { // Select third-person view
                this.setActiveViewCamera(this.thirdPersonViewCamera);
            }
            else if (event.code == this.player.keyCodes.topView && state) { // Select top view
                this.setActiveViewCamera(this.topViewCamera);
            }
            if (event.code == this.player.keyCodes.viewMode && state) { // Single-view mode / multiple-views mode
                this.setViewMode(!this.multipleViewsCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.userInterface && state) { // Display / hide user interface
                this.setUserInterfaceVisibility(!this.userInterfaceCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.miniMap && state) { // Display / hide mini-map
                this.setMiniMapVisibility(!this.miniMapCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.help && state) { // Display / hide help
                this.setHelpVisibility(!this.helpCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.statistics && state) { // Display / hide statistics
                this.setStatisticsVisibility(!this.statisticsCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.run) {
                this.player.keyStates.run = state;
            }
            if (event.code == this.player.keyCodes.left) {
                this.player.keyStates.left = state;
            }
            else if (event.code == this.player.keyCodes.right) {
                this.player.keyStates.right = state;
            }
            if (event.code == this.player.keyCodes.backward) {
                this.player.keyStates.backward = state;
            }
            else if (event.code == this.player.keyCodes.forward) {
                this.player.keyStates.forward = state;
            }
            if (event.code == this.player.keyCodes.jump) {
                this.player.keyStates.jump = state;
            }
            else if (event.code == this.player.keyCodes.yes) {
                this.player.keyStates.yes = state;
            }
            else if (event.code == this.player.keyCodes.no) {
                this.player.keyStates.no = state;
            }
            else if (event.code == this.player.keyCodes.wave) {
                this.player.keyStates.wave = state;
            }
            else if (event.code == this.player.keyCodes.punch) {
                this.player.keyStates.punch = state;
            }
            else if (event.code == this.player.keyCodes.thumbsUp) {
                this.player.keyStates.thumbsUp = state;
            }
        }
    }

    mouseDown(event) {
        if (event.buttons == 1 || event.buttons == 2) { // Primary or secondary button down
            // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
            this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
            // Select the camera whose view is being pointed
            const cameraView = this.getPointedViewport(this.mousePosition);
            if (cameraView != "none") {
                if (cameraView == "mini-map") { // Mini-map camera selected
                    if (event.buttons == 1) { // Primary button down
                        this.dragMiniMap = true;
                    }
                }
                else { // One of the remaining cameras selected
                    const cameraIndex = ["fixed", "first-person", "third-person", "top"].indexOf(cameraView);
                    this.view.options.selectedIndex = cameraIndex;
                    this.setActiveViewCamera([this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera][cameraIndex]);
                    if (event.buttons == 1) { // Primary button down
                        this.changeCameraDistance = true;
                    }
                    else { // Secondary button down
                        this.changeCameraOrientation = true;
                    }
                }
            }
        }
    }

    mouseMove(event) {
        if (event.buttons == 1 || event.buttons == 2) { // Primary or secondary button down
            if (this.changeCameraDistance || this.changeCameraOrientation || this.dragMiniMap) { // Mouse action in progress
                // Compute mouse movement and update mouse position
                const newMousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
                const mouseIncrement = newMousePosition.clone().sub(this.mousePosition);
                this.mousePosition = newMousePosition;
                if (event.buttons == 1) { // Primary button down
                    if (this.changeCameraDistance) {
                        this.activeViewCamera.updateDistance(-0.05 * (mouseIncrement.x + mouseIncrement.y));
                        this.displayPanel();
                    }
                    else if (this.dragMiniMap) {
                        const windowMinSize = Math.min(window.innerWidth, window.innerHeight);
                        const width = this.miniMapCamera.viewport.width * windowMinSize;
                        const height = this.miniMapCamera.viewport.height * windowMinSize;
                        this.miniMapCamera.viewport.x += mouseIncrement.x / (window.innerWidth - width);
                        this.miniMapCamera.viewport.y += mouseIncrement.y / (window.innerHeight - height);
                    }
                }
                else { // Secondary button down
                    if (this.changeCameraOrientation) {
                        this.activeViewCamera.updateOrientation(mouseIncrement.multiply(new THREE.Vector2(-0.5, 0.5)));
                        this.displayPanel();
                    }
                }
            }
        }
    }

    mouseUp(event) {
        // Reset mouse move action
        this.dragMiniMap = false;
        this.changeCameraDistance = false;
        this.changeCameraOrientation = false;
    }

    mouseWheel(event) {
        // Prevent the mouse wheel from scrolling the document's content
        event.preventDefault();
        // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
        this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
        // Select the camera whose view is being pointed
        const cameraView = this.getPointedViewport(this.mousePosition);
        if (cameraView != "none" && cameraView != "mini-map") { // One of the remaining cameras selected
            const cameraIndex = ["fixed", "first-person", "third-person", "top"].indexOf(cameraView);
            this.view.options.selectedIndex = cameraIndex;
            const activeViewCamera = [this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera][cameraIndex];
            activeViewCamera.updateZoom(-0.001 * event.deltaY);
            this.setActiveViewCamera(activeViewCamera);
        }
    }

    contextMenu(event) {
        // Prevent the context menu from appearing when the secondary mouse button is clicked
        event.preventDefault();
    }

    elementChange(event) {
        switch (event.target.id) {
            case "view":
                this.setActiveViewCamera([this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera][this.view.options.selectedIndex]);
                break;
            case "projection":
                this.activeViewCamera.setActiveProjection(["perspective", "orthographic"][this.projection.options.selectedIndex]);
                this.displayPanel();
                break;
            case "horizontal":
            case "vertical":
            case "distance":
            case "zoom":
                if (event.target.checkValidity()) {
                    switch (event.target.id) {
                        case "horizontal":
                        case "vertical":
                            this.activeViewCamera.setOrientation(new Orientation(this.horizontal.value, this.vertical.value));
                            break;
                        case "distance":
                            this.activeViewCamera.setDistance(this.distance.value);
                            break;
                        case "zoom":
                            this.activeViewCamera.setZoom(this.zoom.value);
                            break;
                    }
                }
                break;
            case "multiple-views":
                this.setViewMode(event.target.checked);
                break;
            case "user-interface":
                this.setUserInterfaceVisibility(event.target.checked);
                break;
            case "help":
                this.setHelpVisibility(event.target.checked);
                break;
            case "statistics":
                this.setStatisticsVisibility(event.target.checked);
                break;
        }
    }

    buttonClick(event) {
        switch (event.target.id) {
            case "reset":
                this.activeViewCamera.initialize();
                break;
            case "reset-all":
                this.fixedViewCamera.initialize();
                this.firstPersonViewCamera.initialize();
                this.thirdPersonViewCamera.initialize();
                this.topViewCamera.initialize();
                break;
        }
        this.displayPanel();
    }

    finalSequence() {
        // Disable the fog
        this.fog.enabled = false;
        // Reconfigure the third-person view camera
        this.thirdPersonViewCamera.setOrientation(new Orientation(180.0, this.thirdPersonViewCamera.initialOrientation.v));
        this.thirdPersonViewCamera.setDistance(this.thirdPersonViewCamera.initialDistance);
        this.thirdPersonViewCamera.setZoom(2.0);
        // Set it as the active view camera
        this.setActiveViewCamera(this.thirdPersonViewCamera);
        // Set single-view mode
        this.setViewMode(false);
        // Set the final action
        //this.animations.fadeToAction("Dance", 0.2);
        this.animations.fadeToAction("None", 0.2);

    }
    /*
        collision(position) {
            return this.maze.distanceToWestWall(position) < this.player.radius || this.maze.distanceToEastWall(position) < this.player.radius || this.maze.distanceToNorthWall(position) < this.player.radius || this.maze.distanceToSouthWall(position) < this.player.radius;
        }
    */
    collision(position) {
        return this.maze.distanceToWestWall(position) < this.player.radius / 4 || this.maze.distanceToEastWall(position) < this.player.radius / 4 || this.maze.distanceToNorthWall(position) < this.player.radius / 4 || this.maze.distanceToSouthWall(position) < this.player.radius / 4;
    }


    async update() {

        if (!this.gameRunning) {
            if (this.maze.loaded && this.player.loaded) { // If all resources have been loaded

                // Add the maze, the player and the lights to the scene
                this.scene3D.add(this.maze.object);
                this.scene3D.add(this.player.object);
                this.scene3D.add(this.lights.object);

                // Create the clock
                this.clock = new THREE.Clock();

                // Create model animations (states, emotes and expressions)
                this.animations = new Animations(this.player.object, this.player.animations);

                // Set the player's position and direction
                this.player.position = this.maze.initialPosition.clone();
                this.player.direction = this.maze.initialDirection;

                // Create the user interface
                //this.userInterface = new UserInterface(this.scene3D, this.renderer, this.lights, this.fog, this.player.object, this.animations);

                // Start the game
                this.gameRunning = true;
            }
        }
        else {


            const deltaT = this.clock.getDelta();
            this.animations.update(deltaT);

            // Update the player
            if (!this.animations.actionInProgress) {

                // Check if the player found a bridge
                this.bridgeInfo = this.maze.foundBridge(this.player.position);

                if (this.bridgeInfo ) {

                    console.log("We will change maps according to the bridge connection!");

                    let connectedBuildingCode = this.bridgeInfo.code;
                    let connectedFloorNumber = this.bridgeInfo.floor;
                    let nextMapStartPosition = this.bridgeInfo.inicialPosition;

                    // this.changeMap("./assets/buildings/EdificioB_piso_2.json");

                    this.buildingService.getAllBuildings().subscribe(
                        data => {
                            // Verifica se há dados e faz o find pelo edifício com o código correto                        
                            let connectedBuilding = data.find(building => building.code.includes(connectedBuildingCode));

                            this.floorService.getFloorsAtBuildings(connectedBuilding?.id).subscribe(
                                floorData => {

                                    let connectedFloor = floorData.find(objeto => objeto.floorNumber === connectedFloorNumber);

                                    if (connectedFloor) {

                                        connectedFloor.floorMap.initialPosition = nextMapStartPosition;
                                        this.bridgeCross(connectedFloor);
                                        //await new Promise(resolve => setTimeout(resolve, 5000));
                                        //this.changeMap(connectedFloor);

                                        //this.setActiveViewCamera(this.firstPersonViewCamera);

                                    } else {
                                        if (connectedFloor.length == 0) {
                                            this._snackBar.open("There is no map on any floor of this building", "close", {
                                                duration: 5000,
                                                panelClass: ['snackbar-error']
                                            })
                                        }
                                    };
                                },
                                error => {
                                    this._snackBar.open(error.error, "close", {
                                        duration: 5000,
                                        panelClass: ['snackbar-error']
                                    });
                                }
                            )
                        }).catch(error => {
                            console.error('Error getting building:', error);
                        });


                } else if (this.maze.findElevator(this.player.position) && !this.automaticMode) {
                    this.selectNewFloorFromBuilding();
                } else {
                    let coveredDistance = this.player.walkingSpeed * deltaT;
                    let directionIncrement = this.player.turningSpeed * deltaT;
                    if (this.player.keyStates.run) {
                        coveredDistance *= this.player.runningFactor;
                        directionIncrement *= this.player.runningFactor;
                    }
                    if (this.player.keyStates.left) {
                        this.player.direction += directionIncrement;
                    }
                    else if (this.player.keyStates.right) {
                        this.player.direction -= directionIncrement;
                    }
                    const direction = THREE.MathUtils.degToRad(this.player.direction);
                    if (this.player.keyStates.backward) {
                        const newPosition = new THREE.Vector3(-coveredDistance * Math.sin(direction), 0.0, -coveredDistance * Math.cos(direction)).add(this.player.position);
                        if (this.collision(newPosition)) {
                            // this.animations.fadeToAction("Death", 0.2);
                            this.animations.fadeToAction("None", 0.2);
                        }
                        else {
                            this.animations.fadeToAction(this.player.keyStates.run ? "run" : "walk", 0.2);
                            this.player.position = newPosition;
                        }
                    }
                    else if (this.player.keyStates.forward) {
                        const newPosition = new THREE.Vector3(coveredDistance * Math.sin(direction), 0.0, coveredDistance * Math.cos(direction)).add(this.player.position);
                        if (this.collision(newPosition)) {
                            // this.animations.fadeToAction("Death", 0.2);
                            this.animations.fadeToAction("None", 0.2);
                        }
                        else {
                            this.animations.fadeToAction(this.player.keyStates.run ? "run" : "walk", 0.2);
                            this.player.position = newPosition;
                        }
                    }
                    else {
                        this.animations.fadeToAction("idle", this.animations.activeName != "None" ? 0.2 : 0.6);
                        //this.animations.fadeToAction("Idle", this.animations.activeName != "Death" ? 0.2 : 0.6);
                    }
                    this.player.object.position.set(this.player.position.x, this.player.position.y, this.player.position.z);
                    this.player.object.rotation.y = direction - this.player.initialDirection;
                }
            }

            // Update first-person, third-person and top view cameras parameters (player direction and target)
            this.firstPersonViewCamera.playerDirection = this.player.direction;
            this.thirdPersonViewCamera.playerDirection = this.player.direction;
            this.topViewCamera.playerDirection = this.player.direction;
            const target = new THREE.Vector3(this.player.position.x, this.player.position.y + this.player.eyeHeight, this.player.position.z);
            this.firstPersonViewCamera.setTarget(target);
            this.thirdPersonViewCamera.setTarget(target);
            this.topViewCamera.setTarget(target);

            // Update statistics
            this.statistics.update();

            // Render primary viewport(s)
            this.renderer.clear();

            if (this.fog.enabled) {
                this.scene3D.fog = this.fog.object;
            }
            else {
                this.scene3D.fog = null;
            }
            let cameras;
            if (this.multipleViewsCheckBox.checked) {
                cameras = [this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera];
            }
            else {
                cameras = [this.activeViewCamera];
            }
            for (const camera of cameras) {
                this.player.object.visible = (camera != this.firstPersonViewCamera);
                const viewport = camera.getViewport();
                this.renderer.setViewport(viewport.x, viewport.y, viewport.width, viewport.height);
                this.renderer.render(this.scene3D, camera.object);
                this.renderer.render(this.scene2D, this.camera2D);
                this.renderer.clearDepth();
            }

            // Render secondary viewport (mini-map)
            if (this.miniMapCheckBox.checked) {
                this.scene3D.fog = null;
                this.player.object.visible = true;
                const viewport = this.miniMapCamera.getViewport();
                this.renderer.setViewport(viewport.x, viewport.y, viewport.width, viewport.height);
                this.renderer.render(this.scene3D, this.miniMapCamera.object);
                this.renderer.render(this.scene2D, this.camera2D);
            }
        }
    }
    async changeMap(path, newPosition = null) {
        try {

            if(typeof path == "object"){
                this.floorActual = path;
                this.setActiveViewCamera(this.fixedViewCamera);
                this.gameRunning = false
                this.scene3D.remove(this.maze.object)
                this.mazeParameters.url = path.floorMap;
    
                if(newPosition == null){                
                    this.maze = new Maze(this.mazeParameters);
                }else{
                    //muda a posicao inicial do robot, util para quando sai de elevador ou passagem
                    this.mazeParameters.url.initialPosition = newPosition;
                    
                    this.maze = new Maze(this.mazeParameters); 
                }
            }else{
                this.floorActual = path;
                this.setActiveViewCamera(this.fixedViewCamera);
                this.gameRunning = false
                this.scene3D.remove(this.maze.object)
                this.mazeParameters.url = path;
    
                if(newPosition == null){                
                    this.maze = new Maze(this.mazeParameters);
                }else{
                    //muda a posicao inicial do robot, util para quando sai de elevador ou passagem
                    //this.mazeParameters.url.initialPosition = newPosition;
                    this.mazeParameters.initialPos = newPosition;
                    this.maze = new Maze(this.mazeParameters); 
                    while(!this.maze.loaded){
                        await new Promise(resolve => setTimeout(resolve, 500));
                    }

                }
            }

        } catch (error) {
            console.error('Error changing map:', error);
        }
    }

    /**
     * Recebe um array do tipo
     * [
     *  {caminho:[[x,y]], elevador:boolean, map:json or string}
     * ]
     * @param {*} movementsRobot lista de movimentos em x,y
     * @param {*} inicialPosition posicao para iniciar trajeto
     */
    async performAutomaticMovements(movementsRobot, inicialPosition) {
        let cellDeInicio = inicialPosition;
        for (const numberOfFloors of movementsRobot){
            this.automaticMode = true;
            const movements = this.calculateMovements(cellDeInicio, numberOfFloors.caminho);
            await new Promise(resolve => setTimeout(resolve, 2000));
            
            for (const movement of movements) {
                
                const finalPositiveZ = this.player.position.z + 1;
                const finalNegativeZ = this.player.position.z - 1;
                const finalPositiveX = this.player.position.x + 1;
                const finalNegativeX = this.player.position.x - 1;
                if (movement.Up) {
                    await this.movement(180, finalNegativeZ, finalPositiveZ)
                } else if (movement.Down) {
                    await this.movement(0, finalNegativeZ, finalPositiveZ)
                } else if (movement.Left) {
                    await this.movement(270, finalNegativeX, finalPositiveX)
                } else if (movement.Rigth) {
                    await this.movement(90, finalNegativeX, finalPositiveX)
                } else if (movement.UpRigth) {
                    await this.movementDiagonal(135, finalNegativeX, finalPositiveX, finalNegativeZ, finalPositiveZ)
                } else if (movement.UpLeft) {
                    await this.movementDiagonal(225, finalNegativeX, finalPositiveX, finalNegativeZ, finalPositiveZ)
                } else if (movement.DownRight) {
                    await this.movementDiagonal(45, finalNegativeX, finalPositiveX, finalNegativeZ, finalPositiveZ)
                } else if (movement.DownLeft) {
                    await this.movementDiagonal(315, finalNegativeX, finalPositiveX, finalNegativeZ, finalPositiveZ)
                }
            }
            
            if(numberOfFloors.elevador){
                let light;
                light = this.pontualLight(this.player.position)


                this.setActiveViewCamera(this.firstPersonViewCamera);
                let repeat = 40;
                await new Promise(resolve => setTimeout(resolve, 1000));
                while(repeat>0){
                    this.player.direction += 0.2
                    await new Promise(resolve => setTimeout(resolve, 100));
                    this.player.direction -= 0.2
                    await new Promise(resolve => setTimeout(resolve, 100));
                    repeat--;
                }
                //apagar luz elevador
                this.scene3D.remove(light);
                this.changeMap(numberOfFloors.map, numberOfFloors.initialPosition);

            }

            cellDeInicio = numberOfFloors.initialPosition;

        }

    }

    /**
     * Adiciona luz pontual na posicao que queremos
     * @param {} position 
     * @returns 
     */
    pontualLight(position){
        var light = new THREE.PointLight(0xffffff, 1, 10);
        light.position.copy(position);
        this.scene3D.add(light);
        return light;
    }
    /**
     * Remove luz pontual
     * @param {*} light 
     */
    removeLight(light){
        this.scene3D.remove(light);
    }
    async movement(direction, finalNegative, finalPositive) {
        this.player.direction = direction;
        let reachedFinalX = false;
        let reachedFinalZ = false;
        this.player.keyStates.forward = true;

        do {

            await new Promise(resolve => setTimeout(resolve, 100));
            if (direction == 180 || direction == 0) {
                reachedFinalZ = this.player.position.z >= finalPositive || this.player.position.z <= finalNegative;
            } else {
                reachedFinalX = this.player.position.x >= finalPositive || this.player.position.x <= finalNegative;
            }


        } while (!reachedFinalX && !reachedFinalZ)

        this.player.keyStates.forward = false; // para de movimentar robot para a frente
    }
    async movementDiagonal(direction, finalNegativeX, finalPositiveX, finalNegativeZ, finalPositiveZ) {
        this.player.direction = direction;
        let reachedFinalX = false;
        let reachedFinalZ = false;
        this.player.keyStates.forward = true;

        do {
            await new Promise(resolve => setTimeout(resolve, 5));
            reachedFinalX = this.player.position.x >= finalPositiveX || this.player.position.x <= finalNegativeX;
            reachedFinalZ = this.player.position.z >= finalPositiveZ || this.player.position.z <= finalNegativeZ;

        } while (!reachedFinalX && !reachedFinalZ);

        this.player.keyStates.forward = false; //para de movimentar robot para a frente
    }

    /**
     * Preenche array com movimentacoes do robot
     * @param {*} initialCell 
     * @param {*} destinyCells 
     * @returns 
     */
    calculateMovements(initialCell, destinyCells) {
        const movements = [];
        for (const destinyCell of destinyCells) {
            if (destinyCell[0] > initialCell[0]) {
                if (destinyCell[1] > initialCell[1]) {      //diagonal cima / direita
                    movements.push({ UpRigth: true });
                } else if (destinyCell[1] < initialCell[1]) {//diagonal cima / esquerda
                    movements.push({ UpLeft: true })
                } else {                                  //cima
                    movements.push({ Up: true })
                }
            } else if (destinyCell[0] < initialCell[0]) {
                if (destinyCell[1] > initialCell[1]) {      //diagonal baxo / direita
                    movements.push({ DownRight: true })
                } else if (destinyCell[1] < initialCell[1]) {//diagonal vaixo / esquerda
                    movements.push({ DownLeft: true })
                } else {                                  //baixo
                    movements.push({ Down: true })
                }
            } else if (destinyCell[1] > initialCell[1]) {
                movements.push({ Rigth: true })
            } else if (destinyCell[1] < initialCell[1]) {
                movements.push({ Left: true })
            }

            initialCell = destinyCell; // Atualiza a posição inicial para a célula atual
        }

        return movements;
    }



    listFloorThisBuilding(floors,atualFloor) {
        this.listFloors = floors;
        this.floorActual = atualFloor;
    }

    changeToFixedView(){
        this.setActiveViewCamera(this.fixedViewCamera);
    }
    async selectNewFloorFromBuilding() {
        this.setActiveViewCamera(this.firstPersonViewCamera);
        this.animations.actionInProgress = true;

        this.initializeElevator(
            this.player, this.floorActual, this.animations, this.maze, this.listFloors,this.changeMap.bind(this), this.changeToFixedView.bind(this),
            this.pontualLight.bind(this), this.removeLight.bind(this))

    }

    /**
     * Animacao de elevador
     * Esta a ser criado html e css atraves de javascript
     * Nao mexer , a nao ser que sejas o Miguel Cardoso :D
     * @param {*} player instancia player
     * @param {*} floorActual json floor
     * @param {*} animations instancia animation
     * @param {*} maze instancia maze
     * @param {*} buidlingsFloors floors
     * @param {*} changeMap instancia da funcao changeMap
     */
    async initializeElevator(player,floorActual, animations, maze, buidlingsFloors, changeMap, changeToFixedView,pontualLight,removeLight) {
        var ELEVATOR = {};
        var lightElevator = {};
        var lightOn = true;
        function createElevatorElements() {
            lightElevator = pontualLight(player.position)

            var elevatorPanel = document.createElement('div');
            elevatorPanel.id = 'elevator-panel';
            elevatorPanel.style.background = '#dddddd';
            elevatorPanel.style.border = '5px solid #000000';
            elevatorPanel.style.padding = '30px';
            elevatorPanel.style.width = '30%';
            elevatorPanel.style.height = '70%';
            elevatorPanel.style.zIndex = '9999';
            elevatorPanel.style.position = 'fixed';
            elevatorPanel.style.top = '50%';
            elevatorPanel.style.left = '50%';
            elevatorPanel.style.transform = 'translate(-50%, -50%)';
        
            var displayPanel = document.createElement('div');
            displayPanel.id = 'display-panel';
            displayPanel.style.position = 'relative';
            displayPanel.style.background = '#000000';
            displayPanel.style.overflow = 'hidden';
            displayPanel.style.height = "40%";
        
            var floorNumber = document.createElement('div');
            floorNumber.id = 'floor-number';
            floorNumber.className = 'elevator-info';
            floorNumber.textContent = floorActual.floorNumber;
            floorNumber.style.position = 'absolute';
            floorNumber.style.color = '#ffffff';
            floorNumber.style.fontSize = '20em';
            floorNumber.style.float = 'left';
            floorNumber.style.margin = 'auto';
        
            var directionInfo = document.createElement('div');
            directionInfo.id = 'direction-info';
            directionInfo.className = 'elevator-info';
            directionInfo.style.float = 'right';
            directionInfo.style.height = '100%';
            directionInfo.style.width =  '50%';
            directionInfo.style.display = "grid";
            directionInfo.style.alignItems = "center";
            directionInfo.style.justifyContent = "center";
        
            var upIndicator = document.createElement('div');
            upIndicator.id = 'up-indicator';
            upIndicator.className = 'indicator';
            upIndicator.style.backgroundColor = '#005900';
            upIndicator.style.height = '70px';
            upIndicator.style.width = '70px';
        
            var downIndicator = document.createElement('div');
            downIndicator.id = 'down-indicator';
            downIndicator.className = 'indicator';
            downIndicator.style.backgroundColor = '#990000';
            downIndicator.style.height = '70px';
            downIndicator.style.width = '70px';
        
            var floorSelection = document.createElement('div');
            floorSelection.id = 'floor-selection';
            floorSelection.style.padding = '30px 0';
        
            var navigationList = document.createElement('ul');
            navigationList.id = 'navigation';
            navigationList.style.position = 'relative';

            var arrowElement = document.createElement('div');
            arrowElement.id = 'arrowElementExit'
            arrowElement.style.position = 'absolute';
            arrowElement.style.width = '0';
            arrowElement.style.height = '0';
            arrowElement.style.left = '-15%';
            arrowElement.style.borderLeft = '20px solid transparent';
            arrowElement.style.borderRight = '20px solid transparent';
            arrowElement.style.borderBottom = '40px solid red';
            arrowElement.style.marginBottom = '20px';
            arrowElement.style.transform = 'rotate(-90deg)';
            arrowElement.style.cursor = 'pointer';

            var turnOnLightElement = document.createElement('div');
            turnOnLightElement.id = 'turnOnLight'
            turnOnLightElement.style.position = 'absolute';
            turnOnLightElement.style.width = '0';
            turnOnLightElement.style.height = '0';
            turnOnLightElement.style.left = '43%';
            turnOnLightElement.style.top = '67%';
            turnOnLightElement.style.cursor = 'pointer';

            var image = document.createElement('img');
            image.src = 'assets/butons/light_button.png';

            turnOnLightElement.appendChild(image);
        
            for (var i = 1; i <= 4; i++) {
              var listItem = document.createElement('li');
              listItem.style.position = 'absolute';
              listItem.style.listStyle = 'none';
              listItem.style.width = '110px';
              listItem.style.lineHeight = '100px';
        
              if (i > 2) {
                listItem.style.top = '150px';
              }
        
              if (i % 2 === 0) {
                listItem.style.right = '0';
              } else {
                listItem.style.left = '0';
              }
              var button = document.createElement('div');
              button.className = 'button';
              button.textContent = i;
              button.style.backgroundColor = '#eeeeee';
              button.style.borderRadius = '55px';
              button.style.border = '5px solid #000000';
              button.style.fontSize = '5em';
              button.style.textAlign = 'center';
              button.style.lineHeight = '100px';
              button.style.color = '#000000';
              button.style.cursor = 'pointer';

              listItem.appendChild(button);
              navigationList.appendChild(listItem);
            }
        
            directionInfo.appendChild(upIndicator);
            directionInfo.appendChild(downIndicator);
        
            displayPanel.appendChild(floorNumber);
            displayPanel.appendChild(directionInfo);
        
            elevatorPanel.appendChild(displayPanel);
            elevatorPanel.appendChild(floorSelection);
            elevatorPanel.appendChild(arrowElement);
            elevatorPanel.appendChild(turnOnLightElement);
            floorSelection.appendChild(navigationList);
        
            document.body.appendChild(elevatorPanel);
          }
        
          // Adicionar a criação de elementos antes da lógica existente
        createElevatorElements();

        ELEVATOR.selectedFloorList = [];
        ELEVATOR.$button = document.querySelectorAll('.button');
        ELEVATOR.$floorNumber = document.getElementById('floor-number');
        ELEVATOR.$upIndicator = document.getElementById('up-indicator');
        ELEVATOR.$downIndicator = document.getElementById('down-indicator');
        ELEVATOR.$exitButton = document.getElementById('arrowElementExit');
        ELEVATOR.$turnOnLight = document.getElementById('turnOnLight');
        ELEVATOR.speedFactor = 10000;

      
        ELEVATOR.initialize = function (elevatorSpeed) {
          ELEVATOR.speedFactor = elevatorSpeed;
            
          ELEVATOR.$exitButton.addEventListener('click',function(){
                exitElevator(floorActual.floorMap.elevators[0].exit == undefined ? [0,0] : floorActual.floorMap.elevators[0].exit);
          })

          //funcao para ligar e desligar luz elevador
          ELEVATOR.$turnOnLight.addEventListener('click',function(){
            if(lightOn){
                removeLight(lightElevator);
                lightOn=false;
            }else{
                lightElevator = pontualLight(player.position);
                lightOn=true
            }
        })

          ELEVATOR.$button.forEach(function (button) {
            button.addEventListener('click', function () {
              var selectedFloor = button.textContent;
              var currentFloor = ELEVATOR.$floorNumber.textContent;
      
              if (!button.classList.contains('selectedElevatorButton')) {
                button.classList.add('selectedElevatorButton');
      
                if (selectedFloor === currentFloor) {
                  setTimeout(function () {
                    button.classList.remove('selectedElevatorButton');
                  }, ELEVATOR.speedFactor / 4);
                } else {
                  ELEVATOR.selectedFloorList.push(selectedFloor);
                  if (ELEVATOR.selectedFloorList.length === 1) {

                    changeFloor();
                  }
                }
              }
            });
          });
        };
    
        function exitElevator( position =null){
            //console.log(floorActual.floorMap.elevators[0].exit)
            document.getElementById('elevator-panel').remove();
            changeToFixedView()
            if(position != null){
                player.position = maze.cellToCartesian(position)
            }
            //desliga a luz antes de sair do elevador
            if(lightOn){
                removeLight(lightElevator);
            }

            animations.actionInProgress = false;
        }
        async function changeFloor() {
            
          var selectedFloor = ELEVATOR.selectedFloorList[0];
          var currentFloor = ELEVATOR.$floorNumber.textContent;

          //Up
          if (selectedFloor > currentFloor) {
            ELEVATOR.$upIndicator.style.backgroundColor = '#00cd00';
            animateFloor(
              selectedFloor,
              add(currentFloor),
              '0%',
              '0%',
              add
            );
            await vibracaoCamera();
            floorActual = buidlingsFloors.find(objecto => objecto.floorNumber == selectedFloor);
            await changeMap(floorActual,floorActual.floorMap.elevators[0].exit);
            exitElevator(selectedFloor);

          }
          //DOWN
          else if (selectedFloor < currentFloor) {
            ELEVATOR.$downIndicator.style.backgroundColor = '#ff0000';
            animateFloor(
              selectedFloor,
              subtract(currentFloor),
              '0%',
              '0%',
              subtract
            );
            await vibracaoCamera();
            floorActual = buidlingsFloors.find(objecto => objecto.floorNumber == selectedFloor);
            await changeMap(floorActual, floorActual.floorMap.elevators[0].exit);
            exitElevator(selectedFloor);
            }
        }
      
        function animateFloor(selectedFloor, nextFloor, firstMargin, secondMargin, directionOp) {
          ELEVATOR.$floorNumber.style.marginTop = firstMargin;
      
          setTimeout(function () {
            ELEVATOR.$floorNumber.textContent = nextFloor;
            ELEVATOR.$floorNumber.style.marginTop = secondMargin;
      
            setTimeout(function () {
              if (parseInt(selectedFloor) === nextFloor) {
                // end and change
                setTimeout(function () {
                  ELEVATOR.selectedFloorList.splice(0, 1);
                  resetIndicator();
                  resetButton(selectedFloor);
                  changeFloor();
                }, ELEVATOR.speedFactor);
              } else {
                animateFloor(
                  selectedFloor,
                  directionOp(nextFloor),
                  firstMargin,
                  secondMargin,
                  directionOp
                );
              }
            }, ELEVATOR.speedFactor);
          }, ELEVATOR.speedFactor);
        }
      
        function add(value) {
          return ++value;
        }
      
        function subtract(value) {
          return --value;
        }
      
        function resetIndicator() {
          ELEVATOR.$upIndicator.style.backgroundColor = '#005900';
          ELEVATOR.$downIndicator.style.backgroundColor = '#990000';
        }
      
        function resetButton(selectedFloor) {
          var button = document.querySelector('#navigation li:nth-child(' + selectedFloor + ') .button');
          button.classList.remove('selectedElevatorButton');
        }

        async function vibracaoCamera(){
           
            while(ELEVATOR.$upIndicator.style.backgroundColor == 'rgb(0, 205, 0)' || ELEVATOR.$downIndicator.style.backgroundColor == 'rgb(255, 0, 0)'){
                player.direction += 0.2
                await new Promise(resolve => setTimeout(resolve, 100));
                player.direction -= 0.2
                await new Promise(resolve => setTimeout(resolve, 100));
            }
        }
        ELEVATOR.initialize(1000);  
        return floor;

    }
    
    /**
     * Funcao para animar com feedback visual a passagem entre edificios
     * @param {*} connectedFloor 
     */
    async bridgeCross(connectedFloor){
        this.animations.actionInProgress = true;

        this.bridgeAnimation()
        await new Promise(resolve => setTimeout(resolve, 5000));
        this.changeMap(connectedFloor);

        //this.setActiveViewCamera(this.firstPersonViewCamera);
    }

    /**
     * Feedback basico ao atravessar ponte
     */
    bridgeAnimation(){
        var bridgePanel = document.createElement('div');
        bridgePanel.id = 'bridge-panel';
        bridgePanel.style.background = '#dddddd';
        bridgePanel.style.border = '5px solid #000000';
        bridgePanel.style.padding = '30px';
        bridgePanel.style.width = '70%';
        bridgePanel.style.height = '20%';
        bridgePanel.style.zIndex = '9999';
        bridgePanel.style.position = 'fixed';
        bridgePanel.style.top = '50%';
        bridgePanel.style.left = '50%';
        bridgePanel.style.transform = 'translate(-50%, -50%)';
        bridgePanel.style.fontSize = '40px';
        bridgePanel.style.textAlign = 'center';
        bridgePanel.style.borderRadius = '30px';

        bridgePanel.innerHTML = "Estas a atravessar uma ponte entre pisos<br>Espera <span class='blink'>...</span>";

        document.body.appendChild(bridgePanel);

        //pisca pisca :)
        var style = document.createElement('style');
        style.innerHTML = `
            @keyframes blink {
                0%, 49% {
                    opacity: 1;
                }
                50%, 100% {
                    opacity: 0;
                }
            }
    
            .blink {
                display: inline-block;
                animation: blink 1s step-end infinite;
            }
        `;
        document.head.appendChild(style);

        setTimeout(function () {
            document.body.removeChild(bridgePanel);
        }, 5500);
    }
    
}
