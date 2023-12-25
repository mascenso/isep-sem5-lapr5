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

  public getAllPendingTasks(): Observable<any[]> {
    console.log("sERVICO");
    return this.http.get<any[]>(`${this.API_URL}/api/tasks/pending`);
  }
}
