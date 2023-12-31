import { Component } from '@angular/core';
import { PlaningService } from 'src/app/services/planing.service';
import { TasksService } from 'src/app/services/tasks.service';
import { RobotViewModel } from 'src/app/viewModel/robot';
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
  robot: RobotViewModel[]=[];

  displayedColumns: string[] = ['Nº Gerações', 'Dimensão População', 'Probabilidade Cruzamento(%)', 'Probabilidade Mutacao(%)', 'Tempo limite(s)', 'Avaliação especifica', 'Nº Gerações até estabilização'];
  allSelectedTasks: any[] = [];
  allSelectedTasksInfo: any[] = [];
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
      const endPositionArrayX = endPosition[0];
      const endPositionArrayY = endPosition[1];

      const startPositionArrayX = startPosition[0];
      const startPositionArrayY = startPosition[1];

      return [description, startPositionArrayX, startPositionArrayY, floorsDescription, endPositionArrayX, endPositionArrayY, floorsDescription];
    });
  }

  extractPickupTaskDetails(tasks: TaskPickupViewModel[]): any[] {
    return tasks.map(task => {
      const {
        description,
        pickupLocalization,
        deliveryLocalization
      } = task;

      const pickupRoomX = pickupLocalization.room[0];
      const pickupRoomY = pickupLocalization.room[1];
      const deliveryroomX = deliveryLocalization.room[0];
      const deliveryroomY = deliveryLocalization.room[1];

      const pickupCodeFloor = pickupLocalization.floor;
      const pickupCodes = pickupCodeFloor.code;


      const deliveryCode = deliveryLocalization.floor.code;


      return [description, pickupRoomX, pickupRoomY, pickupCodes, deliveryroomX, deliveryroomY, deliveryCode];
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

    //this.allSelectedTasks = vigilanceTasksInfo.concat(pickUpVigilanceTasksInfo);
    //  this.allSelectedTasks = vigilanceTasksInfo.concat(pickUpVigilanceTasksInfo);

    //this.allSelectedTasksInfo = [this.selectedVigilanceTasks, this.selectedPickupTasks]
    if (vigilanceTasksInfo.length > 0) {
      this.allSelectedTasks = vigilanceTasksInfo;
      this.allSelectedTasksInfo =vigilanceTasksInfo;

    }else {
      this.allSelectedTasks = pickUpVigilanceTasksInfo;
      this.allSelectedTasksInfo =pickUpVigilanceTasksInfo;
    }
    this.allSelectedTasks = this.allSelectedTasks.map((task, index) => {
      const taskIndex = index + 1;

      return [
        `t${taskIndex}`,
        task[1],
        task[2],
        task[3],
        task[4],
        task[5],
        task[6]
      ];

    });

    //para fazer a relação entre a desingação do prolog  e a descrição da task selecionada
    const taskConnections = this.allSelectedTasksInfo.map((task, index) => {
      const taskIndex = index + 1;

      return {
        name: task[0],
        value: `t${taskIndex}`
      };
    });

    console.log("taskConnections ", taskConnections);
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

      const newSeq = this.matchSequenceWithConnections(seq, taskConnections);

      //para mostrar na UI
      obj.sequencia = newSeq;
      this.resultado.sequencia = obj.sequencia;
      this.resultado.tempo = obj.tempo * (this.allSelectedTasks.length / 5);

    });

/*
      this.planingService.getAllRobots().subscribe(robot => {
        this.robot = robot;

        console.log("ROBOTS " ,this.robot );
      });
   */

  }

  matchSequenceWithConnections(sequence: string, connections: any[]) {
    const sequenceParts = sequence.split(' -> ');
    const newSequence = sequenceParts
      .filter(task => connections.some(conn => conn.value === task))
      .map(task =>
        connections.find(conn => conn.value === task).name
      )
      .join(' -> ');

    return newSequence;
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
