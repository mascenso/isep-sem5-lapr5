import { Injectable } from '@angular/core';
import {environment} from "../../environments/environment";
import {HttpClient} from "@angular/common/http";
import {Observable} from "rxjs";
import { TaskVigilanceRequestDTO } from "../../dto/taskVigilanceDTO";
import { TaskPickupRequestDTO } from 'src/dto/taskPickupDTO';

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
    console.log("VIGILANCE SERVIÇO ");

    return this.http.get<TaskVigilanceRequestDTO[]>(`${this.API_URL}/api/tasks/pendingVigilance`);
  }

  public getAllPickupDeliveryPendingTasks(): Observable<TaskPickupRequestDTO[]> {
    console.log("PICK UP SERVIÇO ");

    return this.http.get<TaskPickupRequestDTO[]>(`${this.API_URL}/api/tasks/pendingPickUp`);
  }
/*
  public getAllPendingTasks(): Observable<any[]> {
    let pendingPickUp = this.getAllPickupDeliveryPendingTasks;
    let pendingVigilance = this.getAllVigilancePendingTasks;

    const allPendingTasks: any[] = [ pendingPickUp,pendingVigilance];
    return allPendingTasks;
  }
*/
}
