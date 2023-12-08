import * as THREE from "three";
import Ground from "./ground.js";
import Wall from "./wall.js";
import WallWindow from "./wallWindow.js";
import Door from "./door.js";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";

/*
 * parameters = {
 *  url: String,
 *  credits: String,
 *  scale: Vector3
 * }
 */

export default class Maze {
    constructor(parameters) {
        this.onLoad = function (description) {
            // Store the maze's map and size
            this.map = description.map;
            this.size = description.size;

            // Store the player's initial position and direction
            this.initialPosition = this.cellToCartesian(description.initialPosition);
            this.initialDirection = description.initialDirection;

            // Store the maze's exit location
            this.exitLocation = this.cellToCartesian(description.exitLocation);

            //Store the localion of bridges
            this.bridgesList = [];
            for (let i = 0; i < description.bridges.length; i++) {
                this.bridgesList.push(this.cellToCartesian(description.bridges[i].locationBridge)) 
            }
            
            this.bridgesBuilding = description.bridges;

            // Create a group of objects
            this.object = new THREE.Group();

            // Create the ground
            this.ground = new Ground({ textureUrl: description.groundTextureUrl, size: description.size });
            this.object.add(this.ground.object);

            // Create a wall
            this.wall = new Wall({ textureUrl: description.wallTextureUrl });

            //Create a wall with window
            this.wallWindow = new WallWindow({ textureUrl: description.windowTextureUrl });

            //Create a door
            this.door = new Door({ textureUrl: description.doorTextureUrl });

            for (let i = 0; i <= description.size.width; i++) { // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
                for (let j = 0; j <= description.size.height; j++) { // In order to represent the southmost walls, the map height is one row greater than the actual maze height
                    /*
                     * description.map[][] | Norte wall | Oeste wall| window    | door                      | Elevator  |   Passagem
                     * --------------------+------------+------------------------------------------------------------------------------
                     *          0          |     No     |     No    |   No      |   No                      |   No      |   No
                     *          1          |     No     |    Yes    |   No      |   No                      |   No      |   No
                     *          2          |    Yes     |     No    |   No      |   No                      |   No      |   No
                     *          3          |    Yes     |    Yes    |   No      |   No                      |   No      |   No
                     *        3.1          |    Yes     |    Yes    |   Yes     |   No                      |   No      |   No
                     *        2.1          |    Yes     |    No     |   Yes     |   No                      |   No      |   No
                     *        1.1          |    No      |    Yes    |   Yes     |   No                      |   No      |   No
                     *        1.2          |    No      |    Yes    |   No      |   Yes aberta para norte   |   No      |   No
                     *        1.3          |    No      |    Yes    |   No      |   Yes aberta para sul     |   No      |   No
                     *        0.2          |    No      |    No     |   No      |   Yes aberta para norte   |   No      |   No
                     *        0.3          |    No      |    No     |   No      |   Yes aberta para sul     |   No      |   No
                     *        0.4          |    No      |    No     |   No      |   Yes aberta para oeste   |   No      |   No
                     *        0.5          |    No      |    No     |   No      |   Yes aberta para este    |   No      |   No
                     *        4            |    No      |    No     |   No      |   No                      |   Yes     |   No
                     *        5            |    No      |    No     |   No      |   No                      |   No      |   Yes
                     */
                    switch(description.map[j][i]){
                        case 0:
                            //this.noWalls(this.object);
                            break;
                        case 0.2:
                            this.doorOpenNorth(this.object,this.door,this.wall,i,j,description);
                            break;
                        case 0.3:
                            this.doorOpenSouth(this.object,this.door,this.wall,i,j,description);
                            break;
                        case 0.4:
                            this.doorOpenWest(this.object,this.door,this.wall,i,j,description);
                            break;
                        case 0.5:
                            this.doorOpenEast(this.object,this.door,this.wall,i,j,description); 
                            break;
                        case 1:
                            this.westWall(this.object,this.door,this.wall,i,j,description);
                            break;
                        case 1.1:
                            this.westWallAndWindow(this.object,this.wallWindow,i,j,description);
                            break;
                        case 1.2:
                            this.westWallAndDoorNorth(this.object,this.door,this.wall,i,j,description);
                            break;
                        case 1.3:
                            this.westWallAndDoorSouth(this.object,this.door,this.wall,i,j,description);
                            break;
                        case 2:
                            this.northWall(this.object,this.wall,i,j,description);
                            break;
                        case 2.1:
                            this.northWallAndWindow(this.object,this.wallWindow,i,j,description);
                            break;
                        case 3:
                            this.northAndWestWall(this.object,this.wall,i,j,description);
                            break;
                        case 3.1:
                            this.northAndWestWallAndWindow(this.object,this.wallWindow,i,j,description);
                            break;
                        case 4:
                            this.elevator(this.object,this.elevator,this.wall,i,j,description);
                            break;
                        case 5:
                            this.bridges(this.object,this.ground,i,j,description);
                            break;
                        case 4.1:
                            this.elevatorNorth(this.object,this.elevator,this.wall,i,j,description);
                            break;
                    }
                    
                }
            }

            /**
             * Adicionar cacifos ao piso
             */
            if (description.locker){
                for(let i = 0 ; i< description.locker.length ; i++){
                    const loader = new GLTFLoader();
                    loader.load("./assets/colared_school_lockers__game_ready__4k.glb", (gltf) => {
                        const lockerModel = gltf.scene;
                        // You may need to adjust the position and scale of the model
                        lockerModel.position.set(description.locker[i].position[0] - description.size.width  / 2.0 +0.5 ,0, description.locker[i].position[1] - description.size.height / 2.0-0.25);
                        lockerModel.scale.set(0.5, 0.45, 0.2);
                        this.object.add(lockerModel);
                    });
                }
            }

            /**
             * Adicionar mesas ao piso
             */
            if (description.tables){
                for(let i = 0 ; i< description.tables.length ; i++){
                    const loader = new GLTFLoader();
                    loader.load("./assets/school_desk.glb", (gltf) => {
                        const tableModel = gltf.scene;
                        // You may need to adjust the position and scale of the model
                        tableModel.position.set(description.tables[i].position[0] - description.size.width  / 2.0 +0.5 ,0, description.tables[i].position[1] - description.size.height / 2.0-0.25);
                        tableModel.scale.set(0.005, 0.005, 0.005);
                        tableModel.rotation.y += Math.PI / 1.3
                        tableModel.rotation.y += this.rotation(description.tables[i].orientation)
                        this.object.add(tableModel);
                    });
                }
            }
            
            this.object.scale.set(this.scale.x, this.scale.y, this.scale.z);
            
            this.loaded = true;

        }

        this.onProgress = function (url, xhr) {
            console.log("Resource '" + url + "' " + (100.0 * xhr.loaded / xhr.total).toFixed(0) + "% loaded.");
        }

        this.onError = function (url, error) {
            console.error("Error loading resource " + url + " (" + error + ").");
        }

        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }
        this.loaded = false;

        // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
        THREE.Cache.enabled = true;

        //Esta validação é para ser retrocompativel com a forma que usavamos antes para enviar o mapa
        if(typeof this.url == "string"){
            // Create a resource file loader
            const loader = new THREE.FileLoader();
            
            // Set the response type: the resource file will be parsed with JSON.parse()
            loader.setResponseType("json");
            
            // Load a maze description resource file
            loader.load(
                //Resource URL
                this.url,

                // onLoad callback
                description => this.onLoad(description),

                // onProgress callback
                xhr => this.onProgress(this.url, xhr),

                // onError callback
                error => this.onError(this.url, error)
            );
        }else if(typeof this.url == "object"){
            //como o json é enviado por parametro pode ser feito diretamente o load
            this.onLoad(this.url)

        }


    }

    rotation(orientation){
        switch(orientation){
            case "S":
                return 0;
                break;
            case "E":
                return Math.PI /2;
                break;
            case "N":
                return Math.PI;
                break;
            case "O":
                return Math.PI * 1.5;
                break;
        }
    }

    /**
     * Create a ceel with door open to North
     */
    doorOpenNorth(object,door,wall,i,j,description){
        let doorObject = door.object.clone();
        let wallObject = wall.object.clone();
        //description.size.width+0.5 isto é para encostar a porta a parede e nao ocupar toda a celula
        doorObject.position.set(i - description.size.width  / 2.0 +0.5, 0.5, j - description.size.height / 2.0-0.25);
        wallObject.position.set(i - (description.size.width-0.5)  / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        wallObject.scale.set(0.5,1,1)
        doorObject.scale.set(0.5,1,1)
        doorObject.rotateY(Math.PI / 2.0);
        object.add(doorObject);
        object.add(wallObject);
    }
    /**
     * Create a ceel with door open to South
     */
    doorOpenSouth(object,door,wall,i,j,description){
        let doorObject = door.object.clone();
        let wallObject = wall.object.clone();
        //description.size.width+0.5 isto é para encostar a porta a parede e nao ocupar toda a celula
        doorObject.position.set(i - description.size.width  / 2.0 +0.5, 0.5, j - description.size.height / 2.0+0.25);
        wallObject.position.set(i - (description.size.width-0.5)  / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        wallObject.scale.set(0.5,1,1)
        doorObject.scale.set(0.5,1,1)
        doorObject.rotateY(Math.PI / 2.0);
        object.add(doorObject);
        object.add(wallObject);
    }
    
    /**
     * Create a ceel with door open to West
     */
    doorOpenWest(object,door,wall,i,j,description){
        let doorObject = door.object.clone();
        let wallObject = wall.object.clone();
        //description.size.width+0.5 isto é para encostar a porta a parede e nao ocupar toda a celula
        doorObject.position.set(i - (description.size.width+1.5)  / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        wallObject.position.set(i - (description.size.width+1)  / 2.0 + 0.5, 0.5, j - description.size.height / 2.0+0.75);
        wallObject.scale.set(0.5,1,1)
        doorObject.scale.set(0.5,1,1)
        wallObject.rotateY(Math.PI / 2.0);
        object.add(doorObject);
        object.add(wallObject);
    }

    /**
     * Create a ceel with door open to East
     */
    doorOpenEast(object,door,wall,i,j,description){
        let doorObject = door.object.clone();
        let wallObject = wall.object.clone();
        //description.size.width+0.5 isto é para encostar a porta a parede e nao ocupar toda a celula
        doorObject.position.set(i - (description.size.width+0.5)  / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        wallObject.position.set(i - (description.size.width+1)  / 2.0 + 0.5, 0.5, j - description.size.height / 2.0+0.75);
        wallObject.scale.set(0.5,1,1)
        doorObject.scale.set(0.5,1,1)
        wallObject.rotateY(Math.PI / 2.0);
        object.add(doorObject);
        object.add(wallObject);
    }

    /**
     * Create a Wall on West
     */
    westWall(object,door,wall,i,j,description){
        let wallObject = wall.object.clone();
        wallObject.rotateY(Math.PI / 2.0);
        wallObject.position.set(i - description.size.width / 2.0, 0.5, j - description.size.height / 2.0 + 0.5);
        object.add(wallObject);
    }

    /**
     * Create a Wall and Window on West
     */
    westWallAndWindow(object,wallWindow,i,j,description){
        let wallWindowObject = wallWindow.object.clone();
        wallWindowObject.rotateY(Math.PI / 2.0);
        wallWindowObject.position.set(i - description.size.width / 2.0, 0.5, j - description.size.height / 2.0 + 0.5);
        object.add(wallWindowObject);
    }

    /**
     * Create a Wall and door open to North
     */
    westWallAndDoorNorth(object,door,wall,i,j,description){
        let doorObject = door.object.clone();
        let wallObject = wall.object.clone();
        let wallObject2 = wall.object.clone();
        //description.size.width+0.5 isto é para encostar a porta a parede e nao ocupar toda a celula
        doorObject.position.set(i - description.size.width  / 2.0 +0.5, 0.5, j - description.size.height / 2.0-0.25);
        wallObject.position.set(i - (description.size.width-0.5)  / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        wallObject.scale.set(0.5,1,1)
        doorObject.scale.set(0.5,1,1)
        doorObject.rotateY(Math.PI / 2.0);
        object.add(doorObject);
        object.add(wallObject);

        wallObject2.rotateY(Math.PI / 2.0);
        wallObject2.position.set(i - description.size.width / 2.0, 0.5, j - description.size.height / 2.0 + 0.5);
        object.add(wallObject2);
    }

    /**
     * Create a Wall and door open to South
     */
    westWallAndDoorSouth(object,door,wall,i,j,description){
        let doorObject = door.object.clone();
        let wallObject = wall.object.clone();
        let wallObject2 = wall.object.clone();
        //description.size.width+0.5 isto é para encostar a porta a parede e nao ocupar toda a celula
        doorObject.position.set(i - description.size.width  / 2.0 +0.5, 0.5, j - description.size.height / 2.0+0.25);
        wallObject.position.set(i - (description.size.width-0.5)  / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        wallObject.scale.set(0.5,1,1)
        doorObject.scale.set(0.5,1,1)
        doorObject.rotateY(Math.PI / 2.0);
        object.add(doorObject);
        object.add(wallObject);

        wallObject2.rotateY(Math.PI / 2.0);
        wallObject2.position.set(i - description.size.width / 2.0, 0.5, j - description.size.height / 2.0 + 0.5);
        object.add(wallObject2);
    }

    /**
     * Create a Wall on North
     */
    northWall(object,wall,i,j,description){
        let wallObject = wall.object.clone();
        wallObject.position.set(i - description.size.width / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        object.add(wallObject);
    }

    /**
     * Create a Wall and Window on North
     */
    northWallAndWindow(object,wallWindow,i,j,description){
        let wallWindowObject = wallWindow.object.clone();
        wallWindowObject.position.set(i - description.size.width / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        object.add(wallWindowObject);
    }

    /**
     * Create Wall on North and West
     */
    northAndWestWall(object,wall,i,j,description){
        let wallObject = wall.object.clone();
        let wallObject2 = wall.object.clone();
        wallObject.position.set(i - description.size.width / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        object.add(wallObject);
        wallObject2.rotateY(Math.PI / 2.0);
        wallObject2.position.set(i - description.size.width / 2.0, 0.5, j - description.size.height / 2.0 + 0.5);
        object.add(wallObject2);
    }

    /**
     * Create Wall on North and West and Window
     */
    northAndWestWallAndWindow(object,wallWindow,i,j,description){
        let wallWindowObject = wallWindow.object.clone();
        wallWindowObject.position.set(i - description.size.width / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
        object.add(wallWindowObject);
        let wallWindowObject2 = wallWindow.object.clone();
        wallWindowObject2.rotateY(Math.PI / 2.0);
        wallWindowObject2.position.set(i - description.size.width / 2.0, 0.5, j - description.size.height / 2.0 + 0.5);
        object.add(wallWindowObject2);
    }

    /**
     * Create a elevator
     */
    elevator(object,elevator,wall,i,j,description){
        // Load and add the elevator model to the scene
        const loader = new GLTFLoader();
        let wallObject = wall.object.clone();
        wallObject.position.set(i - (description.size.width-0.5)  / 2.0 + 0.25, 0.5, j - description.size.height / 2.0+1);
        wallObject.scale.set(1,1,1)
        loader.load("./assets/basic_elevator.glb", (gltf) => {
            const elevatorModel = gltf.scene;
            // You may need to adjust the position and scale of the model
            elevatorModel.position.set(i - description.size.width  / 2.0 -0.5 , 1, j - description.size.height / 2.0+1);
            elevatorModel.scale.set(0.0051, 0.005, 0.005);
            this.object.add(elevatorModel);
            });
        object.add(wallObject);   
    }

    /**
     * Create a rotated 180 elevator
     */
    elevatorNorth(object,elevator,wall,i,j,description){
        // Load and add the elevator model to the scene
        const loader = new GLTFLoader();
        let wallObject = wall.object.clone();
        wallObject.position.set(i - (description.size.width-0.5)  / 2.0 + 0.25, 0.5, j - description.size.height / 2.0);
        wallObject.scale.set(1,1,1)
        loader.load("./models/gltf/RobotExpressive/basic_elevator.glb", (gltf) => {
            const elevatorModel = gltf.scene;
            // You may need to adjust the position and scale of the model
            elevatorModel.position.set(i - description.size.width  / 2.0 -0.5 , 1, j - description.size.height / 2.0 - 1);
            elevatorModel.scale.set(0.0051, 0.005, 0.005);
            elevatorModel.rotateY(Math.PI);
            this.object.add(elevatorModel);
            });
        object.add(wallObject);   
    }

    bridges(object,ground,i,j,description){
       
    
    }

    // Convert cell [row, column] coordinates to cartesian (x, y, z) coordinates
    cellToCartesian(position) {
        return new THREE.Vector3((position[1] - this.size.width / 2.0 + 0.5) * this.scale.x, 0.0, (position[0] - this.size.height / 2.0 + 0.5) * this.scale.z)
    }

    // Convert cartesian (x, y, z) coordinates to cell [row, column] coordinates
    cartesianToCell(position) {
        return [Math.floor(position.z / this.scale.z + this.size.height / 2.0), Math.floor(position.x / this.scale.x + this.size.width / 2.0)];
    }

    distanceToWestWall(position) {
        const indices = this.cartesianToCell(position);
        if (this.map[indices[0]][indices[1]] == 1 || this.map[indices[0]][indices[1]] == 3) {
            return position.x - this.cellToCartesian(indices).x + this.scale.x / 2.0;
        }
        return Infinity;
    }

    distanceToEastWall(position) {
        const indices = this.cartesianToCell(position);
        indices[1]++;
        if (this.map[indices[0]][indices[1]] == 1 || this.map[indices[0]][indices[1]] == 3) {
            return this.cellToCartesian(indices).x - this.scale.x / 2.0 - position.x;
        }
        return Infinity;
    }

    distanceToNorthWall(position) {
        const indices = this.cartesianToCell(position);
        if (this.map[indices[0]][indices[1]] == 2 || this.map[indices[0]][indices[1]] == 3) {
            return position.z - this.cellToCartesian(indices).z + this.scale.z / 2.0;
        }
        return Infinity;
    }

    distanceToSouthWall(position) {
        const indices = this.cartesianToCell(position);
        indices[0]++;
        if (this.map[indices[0]][indices[1]] == 2 || this.map[indices[0]][indices[1]] == 3) {
            return this.cellToCartesian(indices).z - this.scale.z / 2.0 - position.z;
        }
        return Infinity;
    }

    foundExit(position) {
        return Math.abs(position.x - this.exitLocation.x) < 0.5 * this.scale.x && Math.abs(position.z - this.exitLocation.z) < 0.5 * this.scale.z;
    };

    //For trigger the new map
    foundBridge(position){
        for (let i = 0; i < this.bridgesList.length; i++) {
            if(Math.abs(position.x - this.bridgesList[i].x) < 0.5 * this.scale.x && Math.abs(position.z - this.bridgesList[i].z) < 0.5 * this.scale.z){
               return true 
            }  
        }
        return false;
    }

    buildingToRender(position){
        for (let i = 0; i < this.bridgesList.length; i++) {
            if(Math.abs(position.x - this.bridgesList[i].x) < 0.5 * this.scale.x && Math.abs(position.z - this.bridgesList[i].z) < 0.5 * this.scale.z){
                return this.bridgesBuilding[i].buildind; 
            }  
        }
        return false;
    }
}