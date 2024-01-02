import { Component, OnDestroy } from '@angular/core';
import { event } from 'cypress/types/jquery';
import { findLastKey } from 'lodash';
import * as caminhos from './caminhos'

@Component({
  selector: 'app-plan-route-with-view',
  templateUrl: './plan-route-with-view.component.html',
  styleUrls: ['./plan-route-with-view.component.css']
})
export class PlanRouteWithViewComponent {
  selectedBuildingInit: any; 
  selectedFloorInit: any;
  selectedBuildingEnd: any;
  selectedFloorEnd: any;
  mapToLoad:any={"data":{}};
  cellsToMove:object[]=[];

  posicaoInicial = [5,10];
  //popular isto com o array de movimentacoes que queremos
  movimentacaoRobotExemplo=caminhos.edAfloor1_edAfloor2;
  

  buildingsInit: any[] = [
    { id: "A", name: 'Edifício A' },
    { id: "B", name: 'Edifício B' },
    { id: "C", name: 'Edifício C' },
  ];

  floorsInit: any[] = [
    { id: "1", floorNumber: 'Piso 1' },
    { id: "2", floorNumber: 'Piso 2' },
  ];

  buildingsEnd: any[] = [
    { id: "A", name: 'Edifício A' },
    { id: "B", name: 'Edifício B' },
    { id: "C", name: 'Edifício C' },
  ];

  floorsEnd: any[] = [
    { id: "1", floorNumber: 'Piso 1' },
    { id: "2", floorNumber: 'Piso 2' },
  ];

  ngOnDestroy(): void {
    this.mapToLoad = {};
  }

  async loadMapAndRoute(){


    const routePlan = "ed"+this.selectedBuildingInit+"floor"+this.selectedFloorInit+"_ed"+this.selectedBuildingEnd+"floor"+this.selectedFloorEnd;
    //substituir aqui com o ficheiro json real
    const mapToLoads = caminhos[routePlan as keyof typeof caminhos];
    console.log(mapToLoads)

    let mapJSON:any={};
    await fetch(mapToLoads[0].openMap!)
      .then(response => {
        if (!response.ok) {
          throw new Error('Erro ao carregar o arquivo JSON');
        }
        return response.json();
      })
      .then(dados => {
        mapJSON = dados;
      })
      .catch(error => {
        console.error(error);
      });

      //colocar uma posicao inicial que faça sentido no futuro
      this.makeInitialPositionAndMove(mapToLoads[0].openMapInitialPosition!,mapJSON,mapToLoads);
    }
    
    //a posicao inicia é [linha,coluna]
    makeInitialPositionAndMove(initialPosition:number[], map:any, cellsToMove:object[]){

      map.initialPosition = initialPosition;
      this.cellsToMove = cellsToMove;
      this.mapToLoad.data = map;

    }

}
