import { Component, OnDestroy } from '@angular/core';
import { event } from 'cypress/types/jquery';

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
  cellsToMove:number[][]=[];

  posicaoInicial = [5,5];
  movimentacaoRobotExemplo=[[5,6],[5,7],[6,7],[6,8],[5,8],[5,9],[5,10],[5,11],[5,12],[5,13],[5,14],[5,15],[5,16],[5,17],[5,18],[5,19],[5,20],[5,21],
                            [5,22],[5,23],[5,24],[5,25]];


  buildingsInit: any[] = [
    { id: 1, name: 'Edifício 1' },
    { id: 2, name: 'Edifício 2' },
  ];

  floorsInit: any[] = [
    { id: 1, floorNumber: 'Piso 1' },
    { id: 2, floorNumber: 'Piso 2' },
  ];

  buildingsEnd: any[] = [
    { id: 1, name: 'Edifício A' },
    { id: 2, name: 'Edifício B' },
  ];

  floorsEnd: any[] = [
    { id: 1, floorNumber: 'Piso X' },
    { id: 2, floorNumber: 'Piso Y' },
  ];

  ngOnDestroy(): void {
    this.mapToLoad = {};
  }

  async loadMapAndRoute(){

    //substituir aqui com o ficheiro json real
    const mapToLoads = 'assets/buildings/EdificioA_piso_2.json';
    let mapJSON:any={};
    await fetch(mapToLoads)
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
      this.makeInitialPositionAndMove(this.posicaoInicial,mapJSON,this.movimentacaoRobotExemplo);
    }
    
    //a posicao inicia é [linha,coluna]
    makeInitialPositionAndMove(initialPosition:number[], map:any, cellsToMove:number[][]){

      map.initialPosition = initialPosition;
      this.cellsToMove = cellsToMove;
      this.mapToLoad.data = map;

    }

}
