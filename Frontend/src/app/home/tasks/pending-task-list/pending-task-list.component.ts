import { Component, OnInit } from "@angular/core";
import { TasksService } from "src/app/services/tasks.service";
import { TaskPickupViewModel } from "src/app/viewModel/taskPickUp";
import { TaskViewModel } from "src/app/viewModel/taskView";
import { TaskVigilanceViewModel } from "src/app/viewModel/taskVigilance";


@Component({
  selector: 'app-pending-task-list',
  templateUrl: './pending-task-list.component.html',
  styleUrls: ['./pending-task-list.component.css']
})

export class PendingTaskListComponent implements OnInit {

  pendingTaskList: TaskViewModel[] = [];
  displayedColumns: string[] = ['description', 'user name', 'user contact'];

  constructor(private tasksService: TasksService) {}

  ngOnInit(): void {
    this.carregarLista();
  }

  carregarLista() {
    this.getPickupTasks();
    this.getVigilanceTasks();
  }

getPickupTasks() {
    this.tasksService.getAllPickupDeliveryPendingTasks().subscribe(
      (pickupTasks) => {
        const pickupTaskList= pickupTasks.flat();

        const pickupTaskViewModels = pickupTaskList.map((task) => new TaskPickupViewModel(task));
        console.log("pickupTaskList ", pickupTaskViewModels);
        this.updatePendingTaskList(pickupTaskViewModels);
      },
      (pickupError) => {
        console.error('Erro ao buscar as tarefas de pick up pendentes:', pickupError);
      }
    );
  }

getVigilanceTasks() {
    this.tasksService.getAllVigilancePendingTasks().subscribe(
      (vigilanceTasks) => {
        const vigilanceTaskList= vigilanceTasks.flat();

        const vigilanceTaskViewModels = vigilanceTaskList.map((task) => new TaskVigilanceViewModel(task));

        console.log("vigilanceTasks ", vigilanceTaskViewModels);

        this.updatePendingTaskList(vigilanceTaskViewModels);
      },
      (vigilanceError) => {
        console.error('Erro ao buscar as tarefas de vigilância pendentes:', vigilanceError);
      }
    );
  }

updatePendingTaskList(tasks: TaskViewModel[]) {
    this.pendingTaskList = this.pendingTaskList.concat(tasks);
    console.log("pendingTaskList ", this.pendingTaskList);

  }
}
