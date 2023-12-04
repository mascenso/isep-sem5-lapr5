import { ErrorHandler, NgModule } from "@angular/core";
import { BrowserModule } from '@angular/platform-browser';
import {FormsModule, ReactiveFormsModule} from '@angular/forms';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { LoginComponent } from './login/login.component';
import {HTTP_INTERCEPTORS, HttpClientModule} from '@angular/common/http';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { HomeComponent } from './home/home.component';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatButtonModule } from '@angular/material/button';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatIconModule } from '@angular/material/icon';
import { MatListModule } from '@angular/material/list';
import { CampusComponent } from './home/campus/campus.component';
import { FleetComponent } from './home/fleet/fleet.component';
import { TasksComponent } from './home/tasks/tasks.component';
import {AuthService} from "./services/auth.service";
import {MatCardModule} from "@angular/material/card";
import {MatInputModule} from "@angular/material/input";
import {MatPaginatorModule} from "@angular/material/paginator";
import {MatTableModule} from "@angular/material/table";
import {MatSortModule} from "@angular/material/sort";
import { CreateBuildingComponent } from './home/campus/create-building/create-building.component';
import {MatRippleModule} from "@angular/material/core";
import {MatSnackBarModule} from "@angular/material/snack-bar";
import { CreateRobotTypeComponent } from './home/fleet/create-robot-type/create-robot-type.component';
import { MatSelectModule } from '@angular/material/select';
import {MatProgressBarModule} from "@angular/material/progress-bar";
import {ServiceInterceptor} from "./interceptors/service.interceptor";

import { FloorListComponent } from './home/campus/floor-list/floor-list.component';
import { BridgeListComponent } from "./home/campus/list-bridge/bridge-list.component";
import { ListBuildingFloorsComponent } from './home/campus/list-building-floors/list-building-floors.component';
import {RobotListComponent} from "./home/fleet/robot-list/robot-list.component";
import { CreateBridgeComponent } from "./home/campus/create-bridge/create-bridge.component";
import { EditFloorsComponent } from "./home/campus/edit-floors/edit-floors.component";
import { GlobalErrorHandler } from "./global-exception";
import { CreateElevatorComponent } from './home/campus/create-elevator/create-elevator.component';
import { ListBuildingsComponent } from './home/campus/list-buildings/list-buildings.component';
import { ListElevatorsComponent } from './home/campus/list-elevators/list-elevators.component';
import { CreateFloorComponent } from './home/campus/create-floor/create-floor.component';
import { FleetListComponent } from './home/fleet/fleet-list/fleet-list.component';
import { AddMapFloorComponent } from './home/campus/add-map-floor/add-map-floor.component';
import { CreateRobotComponent } from './home/fleet/create-robot/create-robot.component';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { EditBridgeComponent } from './home/campus/edit-bridge/edit-bridge.component';
import { EditBuildingComponent } from './home/campus/edit-building/edit-building.component';
import { PlanRobotRouteComponent } from './home/tasks/plan-robot-route/plan-robot-route.component';
import { HttpErrorInterceptor } from "./interceptors/http-error.interceptor";
import { EditElevatorsComponent } from './home/campus/edit-elevators/edit-elevators.component';
import { ViewComponent } from "./home/view/view.component";
import { CreateRoomComponent } from './home/campus/create-room/create-room.component';

@NgModule({
  declarations: [
    AppComponent,
    LoginComponent,
    HomeComponent,
    CampusComponent,
    FleetComponent,
    TasksComponent,
    CreateBuildingComponent,
    CreateBridgeComponent,
    CreateRobotTypeComponent,
    ViewComponent,
    FloorListComponent,
    BridgeListComponent,
    ListBuildingFloorsComponent,
    RobotListComponent,
    EditFloorsComponent,
    CreateElevatorComponent,
    ListBuildingsComponent,
    ListElevatorsComponent,
    CreateFloorComponent,
    FleetListComponent,
    AddMapFloorComponent,
    EditBuildingComponent,
    EditBridgeComponent,
    PlanRobotRouteComponent,
    CreateRobotComponent,
    EditElevatorsComponent,
    CreateRoomComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    FormsModule,
    HttpClientModule,
    BrowserAnimationsModule,
    MatToolbarModule,
    MatButtonModule,
    MatSidenavModule,
    MatIconModule,
    MatInputModule,
    MatCardModule,
    MatSelectModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule,
    MatListModule,
    MatRippleModule,
    ReactiveFormsModule,
    MatSnackBarModule,
    MatProgressBarModule,
    MatSelectModule,
    MatCheckboxModule,
  ],
  providers: [
    {
    provide: HTTP_INTERCEPTORS,
    useClass: ServiceInterceptor,
    multi: true,
    },
    AuthService,
    {
      provide: ErrorHandler,
      useClass: GlobalErrorHandler
    },
    { provide: HTTP_INTERCEPTORS,
      useClass: HttpErrorInterceptor,
      multi: true
    },
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
