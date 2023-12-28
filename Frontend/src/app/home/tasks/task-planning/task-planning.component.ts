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
  inputNGenerations: number = 6;
  inputPopDimensions: number = 8;
  inputPCrossing: number = 50;
  inputPMutations: number = 25;
  inputLTime: number = 2;
  inputTargetEvalution: number = 40;
  inputNGenerationsToStabilization: number = 4;


  resultado: { sequencia: string[], tempo: number } = { sequencia: [], tempo: 0 };


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



      this.resultado = result;
    });

  }

  // Defina displayedColumns e métodos getColumnValue e updateColumnValue no seu componente


  getColumnValue(column: string): any {
    // Ajustando os nomes das propriedades para corresponder aos nomes das colunas
    // Certifique-se de que os nomes correspondam exatamente, caso contrário, ajuste aqui
    const propertyNameMap: { [key: string]: string } = {
      'Nº Gerações': 'inputNGenerations',
      'Dimensão População': 'inputPopDimensions',
      'Probabilidade Cruzamento(%)': 'inputPCrossing',
      'Probabilidade Mutacao(%)': 'inputPMutations',
      'Tempo limite(s)': 'inputLTime',
      'Avaliação especifica': 'inputTargetEvalution',
      'Nº Gerações até estabilização': 'inputNGenerationsToStabilization'
    };
  
    const propertyName = propertyNameMap[column];
    return this[propertyName as keyof TaskPlanningComponent]; // Acessando a propriedade correspondente
  }
  
  updateColumnValue(value: any, column: string): void {
    const propertyNameMap: { [key: string]: string } = {
      'Nº Gerações': 'inputNGenerations',
      'Dimensão População': 'inputPopDimensions',
      'Probabilidade Cruzamento(%)': 'inputPCrossing',
      'Probabilidade Mutacao(%)': 'inputPMutations',
      'Tempo limite(s)': 'inputLTime',
      'Avaliação especifica': 'inputTargetEvalution',
      'Nº Gerações até estabilização': 'inputNGenerationsToStabilization'
    };
  
    const propertyName = propertyNameMap[column];
    this[propertyName as keyof TaskPlanningComponent] = value; // Atualizando a propriedade correspondente
  
    console.log(`Updated ${propertyName} with value: ${value}`); // Adicionando um log para verificar os valores atualizados
  }


}
