import { Component, OnInit } from '@angular/core';
import { TasksService } from 'src/app/services/tasks.service';

@Component({
  selector: 'app-pending-task-list',
  templateUrl: './pending-task-list.component.html',
  styleUrls: ['./pending-task-list.component.css']
})
export class PendingTaskListComponent implements OnInit {

  pendingTaskList: any[] = [];
  displayedColumns: string[] = ['description', 'user name', 'user contact'];

  constructor(private tasksService: TasksService) { }

  ngOnInit(): void {
    this.carregarLista();
  }

  carregarLista() {
    console.log("PASSOU");
    this.tasksService.getAllPendingTasks().subscribe(
      (pendingTaskList) => {
        this.pendingTaskList = pendingTaskList.flat(); // Aplicando flat() para "achatar" os arrays aninhados
        console.log("TASK: ", this.pendingTaskList);
      },
      (error) => {
        console.error('Erro ao buscar as tarefas pendentes:', error);
        // Trate o erro adequadamente, como exibir uma mensagem para o usuário
      }
    );
  }
  

}
