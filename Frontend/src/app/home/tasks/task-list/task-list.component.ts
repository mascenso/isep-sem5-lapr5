import {AfterViewInit, Component, ViewChild} from '@angular/core';
import {FormControl, Validators} from "@angular/forms";
import {TaskStatus} from "../../../model/taskStatus";
import {TaskListResponseDTO} from "../../../../dto/taskListResponseDTO";
import {TasksService} from "../../../services/tasks.service";
import {MatSnackBar} from "@angular/material/snack-bar";
import {take} from "rxjs";
import {MatPaginator} from "@angular/material/paginator";
import {MatTableDataSource} from "@angular/material/table";

@Component({
  selector: 'app-task-list',
  templateUrl: './task-list.component.html',
  styleUrls: ['./task-list.component.css']
})
export class TaskListComponent implements AfterViewInit {

  protected readonly Object = Object;
  public filterOptions = Object.values(FilterOptions);
  public statusOptions = Object.values(TaskStatus);
  filterSelectionControl =  new FormControl();
  emailInputControl= new FormControl('', [Validators.email]);
  selectedFilterOption = '';
  FilterOptions = FilterOptions;
  taskList: TaskListResponseDTO[] = [];
  displayedColumns: string[] = ['description', 'user name', 'user email', 'type'];

  dataSource = new MatTableDataSource<TaskListResponseDTO>(this.taskList);
  @ViewChild(MatPaginator) paginator!: MatPaginator;

  constructor(private taskService: TasksService,
              private _snackBar: MatSnackBar) {}

  ngAfterViewInit(): void {
    this.dataSource.paginator = this.paginator;
  }
  getErrorMessage() {
    return this.emailInputControl.hasError('email') ? 'Not a valid email' : '';
  }

  public setFilterOption(event: any) {
    this.selectedFilterOption = event;
    this.dataSource.data = [];
  }

  public getTasksByStatus(event: any) {
    this.taskService.getTasksByStatus(event)
      .pipe(
        take(1)
      ).subscribe(
      (value) => this.handleResponse(value),
      (error) => this.handleError(error)
    );

  }

  public getTasksByUserEmail(event: any) {
    this.taskService.getTasksByUserEmail(this.emailInputControl.value!)
      .pipe(
        take(1)
      )
      .subscribe(
      (value) => this.handleResponse(value),
      (error) => this.handleError(error)
    );

  }

  private handleResponse(response: TaskListResponseDTO[]) {
    this.taskList = response;
    this.dataSource.data = response;
  }

  private handleError(error: any) {
    console.log(error);
    this._snackBar.open("Something went wrong!", "close", {
      duration: 5000,
      panelClass: ['snackbar-error']
    });
  }


}

export enum FilterOptions {
  USER_EMAIL = "User Email",
  STATUS = "Task Status"
}
