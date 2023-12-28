import { Component } from '@angular/core';
import { PlaningService } from 'src/app/services/planing.service';

@Component({
  selector: 'app-task-planning',
  templateUrl: './task-planning.component.html',
  styleUrls: ['./task-planning.component.css']
})
export class TaskPlanningComponent {

  displayedColumns: string[] = ['Nº Gerações', 'Dimensão População', 'Probabilidade Cruzamento(%)', 'Probabilidade Mutacao(%)', 'Tempo limite(s)', 'Avaliação especifica', 'Nº Gerações até estabilização'];
  tasks = [{ description: 'Tarefa spy 3', value: 't1' }, { name: 'Tarefa spy 4', value: 'a2' }, { name: 'Tarefa uber 3', value: 't4' }, { name: 'Tarefa uber 4', value: 't4' }];
  selectedTasks: string[]=[];
  inputNGenerations: number = 0;
  inputPopDimensions: number = 0;
  inputPCrossing: number = 0;
  inputPMutations: number = 0;
  inputLTime: number = 0;
  inputTargetEvalution: number = 0;
  inputNGenerationsToStabilization: number = 0;

  resultado = [{}];

  constructor(private planingService: PlaningService) { }

  
  // Método para adicionar uma tarefa selecionada ao array
addSelectedTask(task: string) {
  this.selectedTasks.push(task);
}

// Método para remover uma tarefa selecionada do array
removeSelectedTask(task: string) {
  const index = this.selectedTasks.indexOf(task);
  if (index !== -1) {
    this.selectedTasks.splice(index, 1);
  }
}

  planear() {
    //this.planingService.planear(this.selectedTasks, this.inputNGenerations, this.inputPopDimensions, this.inputPCrossing, this.inputPMutations, this.inputLTime,this.inputTargetEvalution, this.inputNGenerationsToStabilization )
    this.planingService.planear(
      this.inputNGenerations,
      this.inputPopDimensions,
      this.inputPCrossing,
      this.inputPMutations,
      this.inputLTime,
      this.inputTargetEvalution,
      this.inputNGenerationsToStabilization
    ).subscribe(result => {
      // Assuming 'result' is an array of objects received from the service
      this.resultado = result; // Assign the received data to 'resultado'
    });

  }

  // Defina displayedColumns e métodos getColumnValue e updateColumnValue no seu componente

  getColumnValue(column: string): any {
    return this[column as keyof TaskPlanningComponent]; // Usa 'keyof' para acessar corretamente as propriedades do componente
  }

  updateColumnValue(value: any, column: string): void {
    this[column as keyof TaskPlanningComponent] = value; // Usa 'keyof' para acessar corretamente as propriedades do componente
  }


}
