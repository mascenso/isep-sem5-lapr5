import { Injectable } from '@angular/core';
import {environment} from "../../environments/environment";
import {HttpClient} from "@angular/common/http";
import {Observable, map} from "rxjs";
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

  /*
  public getAllVigilancePendingTasks(): Observable<TaskVigilanceRequestDTO[]> {
    return this.http.get<TaskVigilanceRequestDTO[]>(`${this.API_URL}/api/tasks/pendingVigilance`);
  }

  public getAllPickupDeliveryPendingTasks(): Observable<TaskPickupRequestDTO[]> {
    return this.http.get<TaskPickupRequestDTO[]>(`${this.API_URL}/api/tasks/pendingPickUp`);
  }
*/


  public getAllPendingTasks(): Observable<TaskViewModel[]> {
    return this.http.get<any[]>(`${this.API_URL}/api/tasks/pending`).pipe(
      map((dataArray: any[][]) => this.mapDataToViewModel(dataArray))
    );
  }

  private mapDataToViewModel(dataArray: any[][]): TaskViewModel[] {
    const tasks: TaskViewModel[] = [];

    dataArray.forEach(dataSet => {
      dataSet.forEach(data => {
        tasks.push(this.mapToViewModel(data));
      });
    });

    return tasks;
  }

  private mapToViewModel(data: any): TaskViewModel {
    console.log('data ', data);
    const userName =data.user.name;
    const userContact = data.user.contacto;

    return {
      id: data.id || '',
      description: data.description || '',
      user: userName,
      contact: userContact,
    };
  }
}