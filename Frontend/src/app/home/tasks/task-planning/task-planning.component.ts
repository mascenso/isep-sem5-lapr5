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

      const floorsDescription = floors && floors.length > 0 ? floors[0].description : '';
      const endPositionArray = Array.isArray(endPosition) ? endPosition : [];
      const startPositionArray = Array.isArray(startPosition) ? startPosition : [];
      console.log('floorsDescription: ', floorsDescription)
      console.log('endPositionArray: ', endPositionArray)

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
      const pickupRoom = Array.isArray(pickupLocalization.room) ? deliveryLocalization.room : [];
      console.log("pickupRoom ", pickupRoom);

      const deliveryroom = Array.isArray(deliveryLocalization.room) ? deliveryLocalization.room : [];
      console.log("deliveryroom ", deliveryroom);
      const pickupCodeFloor = pickupLocalization.floor;
      console.log("pickupCodeFloor ", pickupCodeFloor);
      const pickupCodes = pickupCodeFloor.code;

      console.log("pickupCodes", pickupCodes);

      const deliveryCode = deliveryLocalization.floor.code;
      console.log("deliveryCode", deliveryCode);


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
    console.log("allSelectedTasks", this.allSelectedTasks);

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
