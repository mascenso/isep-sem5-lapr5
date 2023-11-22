import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import {HomeComponent} from "./home/home.component";
import {CampusComponent} from "./home/campus/campus.component";
import {FleetComponent} from "./home/fleet/fleet.component";
import {TasksComponent} from "./home/tasks/tasks.component";
import {ViewComponent} from "./home/view/view.component";
import {LoginComponent} from "./login/login.component";
import {CreateBuildingComponent} from "./home/campus/create-building/create-building.component";
import {CreateRobotTypeComponent} from "./home/fleet/create-robot-type/create-robot-type.component";
import {FloorListComponent} from "./home/campus/floor-list/floor-list.component";
import {CreateFloorComponent} from "./home/campus/create-floor/create-floor.component";
import {BridgeListComponent} from "./home/campus/bridge-list/bridge-list.component";
import { ListBuildingFloorsComponent } from './home/campus/list-building-floors/list-building-floors.component';
import {RobotListComponent} from "./home/fleet/robot-list/robot-list.component";
import { CreateBridgeComponent } from "./home/campus/create-bridge/create-bridge.component";
import { ErrorCheckGuard } from "./error-check.guard";
import { EditFloorsComponent } from './home/campus/edit-floors/edit-floors.component';
import { CreateElevatorComponent } from './home/campus/create-elevator/create-elevator.component';
import {authGuard} from "./guards/auth.guard";
import {ListBuildingsComponent} from "./home/campus/list-buildings/list-buildings.component"

const routes: Routes = [
  {path: '', redirectTo: 'home', pathMatch:'full'},
  {path: 'login', component: LoginComponent},
  {
    path: 'home', component: HomeComponent, canActivate: [authGuard],
    children: [
      {
        path: 'campus',
        component: CampusComponent
      },
      {
        path: 'fleet',
        component: FleetComponent,
      },
      {
        path: 'tasks',
        component: TasksComponent,
      },
      {
        path: 'view',
        component: ViewComponent,
      },
      {
        path: 'campus/create-building',
        component: CreateBuildingComponent,
      },
      {
        path: 'campus/create-bridge',
        component: CreateBridgeComponent,
        canActivate: [ErrorCheckGuard],
      },
      {
        path: 'campus/list-floors',
        component: FloorListComponent,
      },
      {
        path: 'campus/list-bridges',
        component: BridgeListComponent,
      },
      {
        path: 'campus/edit-floors',
        component: EditFloorsComponent,
      },
      {
        path: 'campus/list-building-floors',
        component: ListBuildingFloorsComponent,
      },
      {
        path: 'campus/create-floor',
        component: CreateFloorComponent,
      },
      {
        path: 'campus/create-elevator',
        component: CreateElevatorComponent,
      },
      {
        path: 'campus/list-buildings',
        component: ListBuildingsComponent,
      },
      {
        path: 'fleet/create-robot-type',
        component: CreateRobotTypeComponent,
      },
      {
        path: 'fleet/list-robots',
        component: RobotListComponent,
      },
    ],
  },
  { path: '**', redirectTo: 'home'}
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
