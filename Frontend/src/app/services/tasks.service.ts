import { Injectable } from '@angular/core';
import { environment } from "../../environments/environment";
import { HttpClient } from "@angular/common/http";
import { Observable, forkJoin, map, mergeMap, of } from "rxjs";
import { TaskVigilanceRequestDTO } from "../../dto/taskVigilanceDTO";
import { TaskPickupRequestDTO } from 'src/dto/taskPickupDTO';
import {TaskListResponseDTO} from "../../dto/taskListResponseDTO";

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

  public getAllPickupDeliveryApprovedTasks(): Observable<TaskPickupRequestDTO[]> {
    return this.http.get<TaskPickupRequestDTO[]>(`${this.API_URL}/api/tasks/approvedPickUp`);
  }

  public getAllVigilanceApprovedTasks(): Observable<TaskVigilanceRequestDTO[]> {
    return this.http.get<TaskVigilanceRequestDTO[]>(`${this.API_URL}/api/tasks/approvedVigilance`);
  }

  public updateTaskById(task: TaskVigilanceRequestDTO | TaskPickupRequestDTO): Observable<TaskVigilanceRequestDTO | TaskPickupRequestDTO> {
    return this.http.patch<TaskVigilanceRequestDTO | TaskPickupRequestDTO>(`${this.API_URL}/api/tasks/${task.id}`, task);
  }

  public getTasksByUserEmail(userEmail: string, showSpinner=true): Observable<TaskListResponseDTO[]> {
    return this.http.get<TaskListResponseDTO[]>(`${this.API_URL}/api/tasks/users/${userEmail}`, {reportProgress:showSpinner});
  }

  public getTasksByStatus(status: string, showSpinner=true): Observable<TaskListResponseDTO[]> {
    return this.http.get<TaskListResponseDTO[]>(`${this.API_URL}/api/tasks/status/${status}`, {reportProgress:showSpinner});
  }
}
