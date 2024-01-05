import { Component, OnInit } from "@angular/core";
import { animate, state, style, transition, trigger } from "@angular/animations";
import { TaskViewModel } from "../../../viewModel/taskView";
import { TasksService } from "../../../services/tasks.service";
import { TaskPickupViewModel } from "../../../viewModel/taskPickUp";
import { TaskVigilanceViewModel } from "../../../viewModel/taskVigilance";

@Component({
  selector: 'app-validate-task',
  templateUrl: './validate-task.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
  styleUrls: ['./validate-task.component.css']
})
export class ValidateTaskComponent implements OnInit {

  pendingTaskList: TaskViewModel[] = [];
  displayedColumns: string[] = ['description', 'user name', 'user contact', 'type'];
  expandedElement: any;
  columnsToDisplayWithExpand = [...this.displayedColumns, 'expand'];

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


  acceptTask(task: TaskViewModel): void {
    throw new Error('Method not implemented.');
  }

  rejectTask(task: TaskViewModel) {
    throw new Error('Method not implemented.');
  }
}
