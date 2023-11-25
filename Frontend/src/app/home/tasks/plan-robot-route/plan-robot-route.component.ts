import { Component } from '@angular/core';
import {PlaningService} from '../../../services/planing.service'

@Component({
  selector: 'app-plan-robot-route',
  templateUrl: './plan-robot-route.component.html',
  styleUrls: ['./plan-robot-route.component.css']
})
export class PlanRobotRouteComponent {
  displayedColumns: string[] = ['origem','destino','caminho', 'custo'];
  floors = [{name:'Edificio A Piso 1',value:'a1'}, {name:'Edificio A Piso 2',value:'a2'}, {name:'Edificio B Piso 2',value:'b2'},{name:'Edificio C Piso 3',value:'c3'}]; 
  selectedFloor!: string;
  selectedOtherFloor!: string;
  resultado=[{}];

  constructor(private planingService: PlaningService) {}

  calcular() {
    
    this.planingService.calcular(this.selectedFloor, this.selectedOtherFloor)
      .subscribe(result => {
        //adiciona a origem e destino ao array que vem do swiprolog
        const caminho = {origem:this.floors.find(piso => piso.value === this.selectedFloor)!.name, destino:this.floors.find(piso => piso.value === this.selectedOtherFloor)!.name, ...result}
        //valores para tabela
        this.resultado = [...this.resultado, caminho];

      });  

  
  }
}
