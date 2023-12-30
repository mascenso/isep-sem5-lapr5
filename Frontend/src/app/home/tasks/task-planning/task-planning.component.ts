import { Component } from '@angular/core';
import { PlaningService } from 'src/app/services/planing.service';
import { TasksService } from 'src/app/services/tasks.service';
import { TaskPickupViewModel } from 'src/app/viewModel/taskPickUp';
import { TaskViewModel } from 'src/app/viewModel/taskView';
import { TaskVigilanceViewModel } from 'src/app/viewModel/taskVigilance';

@Component({
  selector: 'app-task-planning',
  templateUrl: './task-planning.component.html',
  styleUrls: ['./task-planning.component.css']
})
export class TaskPlanningComponent {
  pendingTaskList: TaskViewModel[] = [];

  selectedPickupTasks: TaskViewModel[] = [];
  selectedVigilanceTasks: TaskViewModel[] = [];
  approvedVigilanceTasks: TaskViewModel[] = [];
  approvedPickupTasks: TaskViewModel[] = [];

  displayedColumns: string[] = ['Nº Gerações', 'Dimensão População', 'Probabilidade Cruzamento(%)', 'Probabilidade Mutacao(%)', 'Tempo limite(s)', 'Avaliação especifica', 'Nº Gerações até estabilização'];
  selectedTasks: string[]=[];
  inputNGenerations: number = 6;
  inputPopDimensions: number = 8;
  inputPCrossing: number = 50;
  inputPMutations: number = 25;
  inputLTime: number = 1;
  inputTargetEvalution: number = 40;
  inputNGenerationsToStabilization: number = 4;


  resultado: { sequencia: string[], tempo: number } = { sequencia: [], tempo: 0 };


  constructor(
    private planingService: PlaningService,
    private tasksService: TasksService) {     
      this.getApprovedPickupTasks();
      this.getApprovedVigilanceTasks();}


  getApprovedPickupTasks() {
    this.tasksService.getAllPickupDeliveryApprovedTasks().subscribe(
      pickupTasks => {
        const pickupTaskList = pickupTasks.flat();
        const pickupTaskViewModels = pickupTaskList.map((task) =>
          this.mapToTaskViewModel(task, 'Pickup')
        );
        this.approvedPickupTasks = pickupTaskViewModels;
      },
      (pickupError) => {
        console.error(
          'Erro ao buscar as tarefas de pick up aprovadas:',
          pickupError
        );
      }
    );
  }

  getApprovedVigilanceTasks() {
    this.tasksService.getAllVigilanceApprovedTasks().subscribe(
      vigilanceTasks => {
        const vigilanceTasksList = vigilanceTasks.flat();
        const vigilanceTaskViewModels = vigilanceTasksList.map((task) =>
          this.mapToTaskViewModel(task, 'Vigilance')
        );
        this.approvedVigilanceTasks = vigilanceTaskViewModels;
      },
      (vigilanceError) => {
        console.error(
          'Erro ao buscar as tarefas de vigilância aprovadas:',
          vigilanceError
        );
      }
    );
  }

  selectTasksForPlanning(selectedTasks: TaskViewModel[]) {
    // Aqui você pode utilizar a lista de tarefas selecionadas para o planejamento
    console.log('Tarefas selecionadas para o planejamento:', selectedTasks);
  }
/*
  getPickupTasks() {
    this.tasksService.getAllPickupDeliveryApprovedTasks().subscribe(
      pickupTasks => {
        const pickupTaskList = pickupTasks.flat();

        const pickupTaskViewModels = pickupTaskList.map((task) => this.mapToTaskViewModel(task, 'Pickup'));
        this.updatePendingTaskList(pickupTaskViewModels);
      },
      (pickupError) => {
        console.error('Erro ao buscar as tarefas de pick up pendentes:', pickupError);
      }
    );
  }

  getVigilanceTasks() {
    this.tasksService.getAllVigilanceApprovedTasks().subscribe(
      vigilanceTasks => {
        const vigilanceTasksList = vigilanceTasks.flat();

        const vigilanceTaskViewModels = vigilanceTasksList.map((task) => this.mapToTaskViewModel(task, 'Vigilance'));
        this.updatePendingTaskList(vigilanceTaskViewModels);
      },
      (vigilanceError) => {
        console.error('Erro ao buscar as tarefas de vigilância pendentes:', vigilanceError);
      }
    );
  }
*/
  mapToTaskViewModel(task: any, type: 'Pickup' | 'Vigilance'): TaskViewModel {
    let viewModel: TaskViewModel;

    console.log("task ", task);

    if (type === 'Pickup') {
      viewModel = {
        ...task,
        type: 'Pickup'
      } as TaskPickupViewModel;
    } else {
      viewModel = {
        ...task,
        type: 'Vigilance'
      } as TaskVigilanceViewModel;
    }

    return viewModel;
  }
/*
  updatePendingTaskList(tasks: TaskViewModel[]) {
    this.pendingTaskList = this.pendingTaskList.concat(tasks);
  }
  */

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
