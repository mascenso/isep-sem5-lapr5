// @ts-ignore
// @ts-check
// @ts-nocheck
// @ts-expect-error
// @ts-ignore
// @ts-expect-error
import { AfterViewInit, Component, OnDestroy, OnInit } from '@angular/core';
import * as THREE from 'three';
import Orientation from "../../visualisation3d/RobotIsepDrone/orientation.js"
import ThumbRaiser from "../../visualisation3d/RobotIsepDrone/thumb_raiser.js";

@Component({
  selector: 'app-view',
  templateUrl: './view.component.html',
  styleUrls: ['./view.component.css']
})

export class ViewComponent implements OnInit, AfterViewInit,OnDestroy {

  static thumbRaiser: any;
  static mapToRender = "./assets/buildings/EdificioA_piso_2.json";
  static canvas = document.getElementById("canvasForRender");

  ngOnDestroy(): void {
    //throw new Error('Method not implemented.');
  }

  ngAfterViewInit(): void {
    this.namesToDropDown();
    this.initialize();
    ViewComponent.animate();
  }

  ngOnInit(): void {
    // throw new Error('Method not implemented.');
  }


   initialize() {
    // Create the game
    ViewComponent.thumbRaiser = new ThumbRaiser(
      {}, // General Parameters
      { url:ViewComponent.mapToRender ,scale: new THREE.Vector3(1.0, 0.5, 1.0),  }, // Maze parameters
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

    );

  }

  static animate() {
    requestAnimationFrame(ViewComponent.animate);
    // Update the game
    ViewComponent.thumbRaiser.update()
  }

  namesToDropDown() {
    const names = [
      {name: "Edificio A piso 2", fileName:'./assets/buildings/EdificioA_piso_2.json'},
      {name: "Edificio B piso 1", fileName:'./assets/buildings/EdificioB_piso_1.json'},
      {name: "Edificio B piso 2", fileName:'./assets/buildings/EdificioB_piso_2.json'},
      {name: "Edificio C piso 2", fileName:'./assets/buildings/EdificioC_piso_2.json'}
    ]
    const dropdown = document.getElementById('floor-selector');

    try {
      names.forEach(fileName => {
        const option = document.createElement('option');
        option.value = fileName.fileName;
        option.text = fileName.name;
        dropdown!.add(option);
      });
    } catch (error) {
      console.error('Erro ao carregar nomes de arquivos JSON:', error);
    }
  }

  //coloca a funcao global para ser ouvida no on change
  changeFloor(path: any){
    ViewComponent.thumbRaiser.maze.url = path.target.value;
    ViewComponent.thumbRaiser.changeMap(path.target.value);
  }



}
