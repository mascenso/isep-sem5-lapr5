import { Component, OnInit } from '@angular/core';
import { TasksService } from 'src/app/services/tasks.service';
import { TaskViewModel } from 'src/app/viewModel/taskView';
import { TaskPickupViewModel } from 'src/app/viewModel/taskPickUp';
import { TaskVigilanceViewModel } from 'src/app/viewModel/taskVigilance';

@Component({
  selector: 'app-pending-task-list',
  templateUrl: './pending-task-list.component.html',
  styleUrls: ['./pending-task-list.component.css']
})
export class PendingTaskListComponent implements OnInit {

  pendingTaskList: TaskViewModel[] = [];
  displayedColumns: string[] = ['description', 'user name', 'user contact', 'type'];

  constructor(private tasksService: TasksService) { }

  ngOnInit(): void {
    this.carregarLista();
  }

  carregarLista() {
    this.getPickupTasks();
    this.getVigilanceTasks();
  }

  getPickupTasks() {
    this.tasksService.getAllPickupDeliveryPendingTasks().subscribe(
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
    this.tasksService.getAllVigilancePendingTasks().subscribe(
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

  updatePendingTaskList(tasks: TaskViewModel[]) {
    this.pendingTaskList = this.pendingTaskList.concat(tasks);
  }
}
