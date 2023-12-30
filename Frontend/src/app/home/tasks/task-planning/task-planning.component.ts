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
  inputLTime: number = 1;
  inputTargetEvalution: number = 40;
  inputNGenerationsToStabilization: number = 4;


  resultado: { sequencia: string[], tempo: number } = { sequencia: [], tempo: 0 };


  constructor(private planingService: PlaningService) { }


  planear() {
    const taskParameters = {
      Ngeracoes: this.inputNGenerations,
      dimensaoPop: this.inputPopDimensions,
      pobCruz: this.inputPCrossing,
      pobMut: this.inputPMutations,
      tempoLimite: this.inputLTime,
      avaliacaoDef: this.inputTargetEvalution,
      nEstabiliz: this.inputNGenerationsToStabilization
    };
    //this.planingService.planear(this.selectedTasks, this.inputNGenerations, this.inputPopDimensions, this.inputPCrossing, this.inputPMutations, this.inputLTime,this.inputTargetEvalution, this.inputNGenerationsToStabilization )
    this.planingService.planear(taskParameters).subscribe(result => {
      const json = result.substring(result.indexOf('{'));

      let obj = JSON.parse(json);
      const seq = obj.sequencia.join(' -> ');
      obj.sequencia=seq;

      this.resultado.sequencia = obj.sequencia;
      this.resultado.tempo = obj.tempo;
    });

  }

  getColumnValue(column: string): any {
    // Ajustando os nomes das propriedades para corresponder aos nomes das colunas
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
