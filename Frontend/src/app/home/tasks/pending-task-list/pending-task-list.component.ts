// pending-task-list.component.ts
import { Component, OnInit } from "@angular/core";
import { TaskViewModel } from "src/app/viewModel/taskView";
import { TasksService } from "src/app/services/tasks.service";

@Component({
  selector: 'app-pending-task-list',
  templateUrl: './pending-task-list.component.html',
  styleUrls: ['./pending-task-list.component.css']
})
export class PendingTaskListComponent implements OnInit {
  pendingTaskList: TaskViewModel[] = [];
  displayedColumns: string[] = ['description', 'user name', 'user contact'];

  constructor(private taskService: TasksService) {}

  ngOnInit(): void {
    this.taskService.getAllPendingTasks().subscribe(
      (tasks) => {

        this.pendingTaskList = tasks;
      },
      (error) => {
        console.error('Erro ao buscar as tarefas pendentes:', error);
      }
    );
  }
}
