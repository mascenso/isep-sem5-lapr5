import { Injectable } from '@angular/core';
import { environment } from "../../environments/environment";
import { HttpClient } from "@angular/common/http";
import { Observable, forkJoin, map, mergeMap, of } from "rxjs";
import { TaskVigilanceRequestDTO } from "../../dto/taskVigilanceDTO";
import { TaskPickupRequestDTO } from 'src/dto/taskPickupDTO';
import { TaskViewModel } from '../viewModel/taskView';

@Injectable({
  providedIn: 'root'
})
export class TasksService {

  private API_URL = environment.API_URL_TASKS;

  constructor(private http: HttpClient) { }

  public createVigilanceTask(vigilanceTask: TaskVigilanceRequestDTO): Observable<TaskVigilanceRequestDTO[]> {
    return this.http.post<TaskVigilanceRequestDTO[]>(`${this.API_URL}/api/tasks/vigilance`, vigilanceTask);
  }

  public createPickupTask(pickupTask: TaskPickupRequestDTO): Observable<TaskPickupRequestDTO[]> {
    return this.http.post<TaskPickupRequestDTO[]>(`${this.API_URL}/api/tasks/pickupDelivery`, pickupTask);
  }


  public getAllVigilancePendingTasks(): Observable<TaskVigilanceRequestDTO[]> {
    return this.http.get<TaskVigilanceRequestDTO[]>(`${this.API_URL}/api/tasks/pendingVigilance`);
  }
  
  public getAllPickupDeliveryPendingTasks(): Observable<TaskPickupRequestDTO[]> {
    return this.http.get<TaskPickupRequestDTO[]>(`${this.API_URL}/api/tasks/pendingPickUp`);
  }


 /* 
  public getAllPendingTasks(): Observable<TaskViewModel[]> {
    const pickupTasks$ = this.getAllPickupDeliveryPendingTasks().pipe(
      mergeMap(tasks => this.mapToViewModelPickUp(tasks)),
    );
  
    const vigilanceTasks$ = this.getAllVigilancePendingTasks().pipe(
      mergeMap(tasks => this.mapToViewModelVigilance(tasks)),
    );
  
    // Combine the results using forkJoin to wait for both observables to complete
    return forkJoin([pickupTasks$, vigilanceTasks$]).pipe(
      // Merge the arrays into a single array of TaskViewModel
      map(([pickupTasksResult, vigilanceTasksResult]) => [...pickupTasksResult, ...vigilanceTasksResult]),
    );
  }

  private mapToViewModelPickUp(dataArray: TaskPickupRequestDTO[]): Observable<TaskViewModel[]> {
    return of(dataArray.map(dataSet => this.mapToViewModel(dataSet)));
  }
  
  private mapToViewModelVigilance(dataArray: TaskVigilanceRequestDTO[]): Observable<TaskViewModel[]> {
    return of(dataArray.map(dataSet => this.mapToViewModel(dataSet)));
  }


  private mapToViewModelPickUp(dataArray: TaskPickupRequestDTO[]): TaskViewModel[] {
    const tasks: TaskViewModel[] = [];
    dataArray.forEach(dataSet => {
        tasks.push(this.mapToViewModel(dataSet));
    });
    return tasks;
  }

  private mapToViewModelVigilance(dataArray: TaskVigilanceRequestDTO[]): TaskViewModel[] {
    const tasks: TaskViewModel[] = [];
    dataArray.forEach(dataSet => {
        tasks.push(this.mapToViewModel(dataSet));
    });
    return tasks;
  }
  
  private mapToViewModel(dataArray: any[]): TaskViewModel[] {
    const result: TaskViewModel[] = [];
  
    dataArray.forEach(data => {
      // Aqui, data representa um item do array dentro do array maior
      const mappedData = this.mapSingleToViewModel(data);
      result.push(mappedData);
    });
  
    return result;
  }
  
  private mapSingleToViewModel(data: any): TaskViewModel {

    console.log("final ", data.user.name);
    // Aqui, data representa um único item do array
    return {
      id: data.id,
      description: data.description,
      user: data.user.name,
      contact: data.user.contact,
      // Mapear outras propriedades conforme necessário
    };
  }
*/
}
