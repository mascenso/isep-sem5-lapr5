// @ts-ignore
// @ts-check
// @ts-nocheck
// @ts-expect-error
// @ts-ignore
// @ts-expect-error
import { AfterViewInit, Component, OnDestroy, OnInit, Input } from '@angular/core';
import * as THREE from 'three';
import Orientation from "../../visualisation3d/RobotIsepDrone/orientation.js"
import ThumbRaiser from "../../visualisation3d/RobotIsepDrone/thumb_raiser.js";
import { BuildingService } from '../../services/building.service.ts';
import { FloorService } from '../../services/floor.service.ts';
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { FloorResponseDTO } from "../../../dto/floorDTO"
import { MatSnackBar } from "@angular/material/snack-bar";
import { ConsoleReporter } from 'jasmine';

@Component({
  selector: 'app-view',
  templateUrl: './view.component.html',
  styleUrls: ['./view.component.css']
})

export class ViewComponent implements OnInit {
  @Input() showMenus = true;
  @Input() automaticPlaning: any = {};
  @Input() cellsToMove: number[][] = [];

  //@Input() mapToRender: any = "";
  thumbRaiser: any;
  buildings: BuildingResponseDTO[] = [];

  //apenas mostra andares que tem mapa de piso
  floors: FloorResponseDTO[] = [];
  buildingSelected = "";
  floorSelected = "";
  floorMap: any;
  mapToRender = "";
  //static canvas = document.getElementById("canvasForRender");

  constructor(private buildingService: BuildingService, private floorService: FloorService, private _snackBar: MatSnackBar) { }

  ngOnInit(): void {
    this.buildingsToDropDown();
    this.initialize();
    this.animate();
  }
  ngAfterViewInit() {
    this.haveMap();
  }

  initialize() {
    // Create the game
    this.thumbRaiser = new ThumbRaiser(
      {}, // General Parameters
      { url: this.mapToRender, scale: new THREE.Vector3(1.0, 0.5, 1.0), }, // Maze parameters
      {}, // Player parameters
      { ambientLight: { intensity: 0.1 }, pointLight1: { intensity: 50.0, distance: 20.0, position: new THREE.Vector3(-3.5, 10.0, 2.5) }, pointLight2: { intensity: 50.0, distance: 20.0, position: new THREE.Vector3(3.5, 10.0, -2.5) } }, // Lights parameters
      {}, // Fog parameters
      { view: "fixed", multipleViewsViewport: new THREE.Vector4(0.0, 1.0, 0.45, 0.5) }, // Fixed view camera parameters
      { view: "first-person", multipleViewsViewport: new THREE.Vector4(1.0, 1.0, 0.55, 0.5), initialOrientation: new Orientation(0.0, -10.0), initialDistance: 2.0, distanceMin: 1.0, distanceMax: 4.0 }, // First-person view camera parameters
      { view: "third-person", multipleViewsViewport: new THREE.Vector4(0.0, 0.0, 0.55, 0.5), initialOrientation: new Orientation(0.0, -20.0), initialDistance: 2.0, distanceMin: 1.0, distanceMax: 4.0 }, // Third-person view camera parameters
      { view: "top", multipleViewsViewport: new THREE.Vector4(1.0, 0.0, 0.45, 0.5), initialOrientation: new Orientation(0.0, -90.0), initialDistance: 4.0, distanceMin: 1.0, distanceMax: 16.0 }, // Top view camera parameters
      { view: "mini-map", multipleViewsViewport: new THREE.Vector4(0.99, 0.02, 0.3, 0.3), initialOrientation: new Orientation(180.0, -90.0), initialZoom: 0.29 }, // Mini-msp view camera parameters
      {
        skyboxes: [
          { // Stormy days
            name: "Stormy days",
            texturePath: "./assets/cube_textures/envmap_stormydays/",
            texturePositiveXUrl: "stormydays_ft.jpg",
            textureNegativeXUrl: "stormydays_bk.jpg",
            texturePositiveYUrl: "stormydays_up.jpg",
            textureNegativeYUrl: "stormydays_dn.jpg",
            texturePositiveZUrl: "stormydays_rt.jpg",
            textureNegativeZUrl: "stormydays_lf.jpg",
            credits: "Skybox created by <a href='https://opengameart.org/content/stormy-days-skybox' target='_blank' rel='noopener'>Jockum Skoglund (hipshot)</a>."
          }
        ],
        selected: 0
      }, // Cube texture parameters
      this.buildingService,
      this.floorService
    );


  }

  animate() {
    requestAnimationFrame(this.animate.bind(this));
    // Update the game
    //console.log(this.thumbRaiser.player.position)
  /*  let idx = this.thumbRaiser.isABridge();
    if (idx != -1) {
      this.nextMapByBridge(idx);
      console.log("APOS nextMapByBridge");
    }
*/
    // Update the game
    this.thumbRaiser.update()

  }

  buildingsToDropDown() {
    this.buildingService.getAllBuildings().subscribe(
      buildings => {
        this.buildings = buildings;
      },
      error => {
        this._snackBar.open(error.error, "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
      }
    );
  }

  floorsToDropdown() {
    this.floorService.getFloorsAtBuildings(this.buildingSelected).subscribe(
      floorData => {
        this.floors = floorData.filter(objeto => objeto.floorMap != undefined && objeto.floorMap.initialPosition != undefined);

        if (this.floors.length == 0) {
          this._snackBar.open("There is no map on any floor of this building", "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
        }
      },
      error => {
        this._snackBar.open(error.error, "close", {
          duration: 5000,
          panelClass: ['snackbar-error']
        });
      }
    )

  }

  //coloca a funcao global para ser ouvida no on change
  changeFloor() {
    if (this.floorSelected) {
      this.mapToRender = this.floors.find(objeto => objeto.id === this.floorSelected)?.floorMap;
    }

    this.thumbRaiser.maze.url = this.mapToRender;
    //this.thumbRaiser.listFloorThisBuilding(this.floors) ;
    this.thumbRaiser.changeMap(this.mapToRender);
  }
  async haveMap() {
    if (this.automaticPlaning.data != undefined && this.cellsToMove.length > 0) {
      this.makeAutomaticAnimation(this.cellsToMove)
    }
  }

  makeAutomaticAnimation(cellsToMove: number[][]) {
    //mapa (JSON) passado por parametro no componente
    this.mapToRender = this.automaticPlaning.data;

    this.thumbRaiser.maze.url = this.mapToRender;
    this.thumbRaiser.changeMap(this.mapToRender);

    this.thumbRaiser.performAutomaticMovements(cellsToMove, this.mapToRender.initialPosition);
  }

  nextMapByBridge(index: number) {
    //this.changeMap("./assets/buildings/EdificioB_piso_2.json");
/*
    console.log("Iremos trocar de mapa de acordo com a ligação da bridge!", index);
   // console.log("MAPA ATUAL", this.mapToRender);

   let connectedBuildingCode = this.mapToRender.bridges[index].buildind.code;
   let connectedFloorNumber = this.mapToRender.bridges[index].buildind.floor;
   let nextMapStartPosition = this.mapToRender.bridges[index].buildind.inicialPosition;


   console.log("BuildingCode",connectedBuildingCode);
   
    this.buildingService.getAllBuildings().subscribe(
      data => {
        // Verifica se há dados e filtra pelo edifício com o código correto                        
        let connectedBuilding = data.find(building => building.code.includes(connectedBuildingCode));
        console.log("connectedBuilding #### ",connectedBuilding);

        this.floorService.getFloorsAtBuildings(connectedBuilding?.id).subscribe(
          floorData => {

            let connectedFloor = floorData.find(objeto => objeto.floorNumber === connectedFloorNumber);
            console.log("connectedFloor #### ",connectedFloor);

            if (connectedFloor) {
              this.mapToRender = this.floors.find(objeto => objeto.id === connectedFloor?.id)?.floorMap;
              console.log("NOVO MAPA", this.mapToRender);

              this.mapToRender.inicialPosition = nextMapStartPosition;
              this.thumbRaiser.maze.url = this.mapToRender;

              console.log("#### MAPA ####",this.thumbRaiser.maze.url);
              //this.thumbRaiser.changeMap(this.mapToRender);
              //console.log("#### GAME OVER ####", this.thumbRaiser.changeMap(this.mapToRender));
                  this.changeMap("./assets/buildings/EdificioB_piso_2.json");

            }else{ 
              if (connectedFloor.length == 0) {
              this._snackBar.open("There is no map on any floor of this building", "close", {
                duration: 5000,
                panelClass: ['snackbar-error']
              })}
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


*/

    //this.changeMap("./assets/buildings/EdificioB_piso_2.json");

  }
}
