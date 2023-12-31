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

  selectedPickupTasks: TaskPickupViewModel[] = [];
  selectedVigilanceTasks: TaskVigilanceViewModel[] = [];
  approvedVigilanceTasks: TaskViewModel[] = [];
  approvedPickupTasks: TaskViewModel[] = [];

  displayedColumns: string[] = ['Nº Gerações', 'Dimensão População', 'Probabilidade Cruzamento(%)', 'Probabilidade Mutacao(%)', 'Tempo limite(s)', 'Avaliação especifica', 'Nº Gerações até estabilização'];
  allSelectedTasks: any[] = [];
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
    this.getApprovedVigilanceTasks();
  }



  extractVigilanceTaskDetails(tasks: TaskVigilanceViewModel[]): any[] {
    return tasks.map(task => {
      const { description, floors, endPosition, startPosition } = task;
      const floorsDescription = floors[0].description;
      const endPositionArray = JSON.stringify(endPosition);
      console.log("endPositionArray ", endPositionArray);
      const startPositionArray = JSON.stringify(startPosition);

      return [description, startPositionArray, floorsDescription, endPositionArray, floorsDescription];
    });
  }

  extractPickupTaskDetails(tasks: TaskPickupViewModel[]): any[] {
    return tasks.map(task => {
      const {
        description,
        pickupLocalization,
        deliveryLocalization
      } = task;

      const pickupRoom = JSON.stringify(pickupLocalization.room);
      const deliveryroom = JSON.stringify(deliveryLocalization.room);
      const pickupCodeFloor = pickupLocalization.floor;
      const pickupCodes = pickupCodeFloor.code;


      const deliveryCode = deliveryLocalization.floor.code;


      return [description, pickupRoom, pickupCodes, deliveryroom, deliveryCode];
    });
  }

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

  mapToTaskViewModel(task: any, type: 'Pickup' | 'Vigilance'): TaskViewModel {
    let viewModel: TaskViewModel;

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


  planear() {
    const vigilanceTasksInfo = this.extractVigilanceTaskDetails(this.selectedVigilanceTasks);
    const pickUpVigilanceTasksInfo = this.extractPickupTaskDetails(this.selectedPickupTasks);

    this.allSelectedTasks = vigilanceTasksInfo.concat(pickUpVigilanceTasksInfo);  
    
    this.allSelectedTasks = this.allSelectedTasks.map((task, index) => {
      const taskIndex = index + 1; // Ou alguma lógica para obter o tXX correto
      return [
        `t${taskIndex}`,
        task[1], // Coordenadas [2, 4], [2, 3], etc.
        task[2], // Algum valor correspondente, como 'a1', 'a2', etc.
        task[3], // Outra coordenada [3, 3], [2, 2], etc.
        task[4]  // Outro valor correspondente, como 'a1', 'a2', etc.
      ];
    });
    
    console.log("allSelectedTasks 2 ", this.allSelectedTasks);

    const taskParameters = {
      LTasks: this.allSelectedTasks,
      Ngeracoes: this.inputNGenerations,
      dimensaoPop: this.inputPopDimensions,
      pobCruz: this.inputPCrossing,
      pobMut: this.inputPMutations,
      tempoLimite: this.inputLTime,
      avaliacaoDef: this.inputTargetEvalution,
      nEstabiliz: this.inputNGenerationsToStabilization
    };


    this.planingService.planear(taskParameters).subscribe(result => {
      const json = result.substring(result.indexOf('{'));

      let obj = JSON.parse(json);
      const seq = obj.sequencia.join(' -> ');
      obj.sequencia = seq;

      this.resultado.sequencia = obj.sequencia;
      this.resultado.tempo = obj.tempo;
    });



  }

  getColumnValue(column: string): any {
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
    return this[propertyName as keyof TaskPlanningComponent]; // Aceder a propriedade correspondente
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
    this[propertyName as keyof TaskPlanningComponent] = value; // Atualiza a propriedade correspondente

    console.log(`Updated ${propertyName} with value: ${value}`); // Adiciona um log para verificar os valores atualizados
  }


}
