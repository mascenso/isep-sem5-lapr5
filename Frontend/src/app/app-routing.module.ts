import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import {HomeComponent} from "./home/home.component";
import {UsersComponent} from "./home/users/users.component";
import {ValidateUserComponent} from "./home/users/validate-user/validate-user.component";
import {CreateUserComponent} from "./home/users/create-user/create-user.component";
import {CampusComponent} from "./home/campus/campus.component";
import {FleetComponent} from "./home/fleet/fleet.component";
import {TasksComponent} from "./home/tasks/tasks.component";
import {ViewComponent} from "./home/view/view.component";
import {LoginComponent} from "./login/login.component";
import {RegisterComponent} from "./register/register.component";
import {CreateBuildingComponent} from "./home/campus/create-building/create-building.component";
import {CreateRobotTypeComponent} from "./home/fleet/create-robot-type/create-robot-type.component";
import {FloorListComponent} from "./home/campus/floor-list/floor-list.component";
import {CreateFloorComponent} from "./home/campus/create-floor/create-floor.component";
import {AddMapFloorComponent} from "./home/campus/add-map-floor/add-map-floor.component"
import {BridgeListComponent} from "./home/campus/list-bridge/bridge-list.component";
import { ListBuildingFloorsComponent } from './home/campus/list-building-floors/list-building-floors.component';
import {RobotListComponent} from "./home/fleet/robot-list/robot-list.component";
import { CreateBridgeComponent } from "./home/campus/create-bridge/create-bridge.component";
import { ErrorCheckGuard } from "./error-check.guard";
import { EditFloorsComponent } from './home/campus/edit-floors/edit-floors.component';
import { CreateElevatorComponent } from './home/campus/create-elevator/create-elevator.component';
import {authGuard} from "./guards/auth.guard";
import {ListBuildingsComponent} from "./home/campus/list-buildings/list-buildings.component"
import { ListElevatorsComponent } from './home/campus/list-elevators/list-elevators.component';
import { FleetListComponent } from './home/fleet/fleet-list/fleet-list.component';
import { CreateRobotComponent } from './home/fleet/create-robot/create-robot.component';
import {EditBridgeComponent} from './home/campus/edit-bridge/edit-bridge.component'
import {EditBuildingComponent} from './home/campus/edit-building/edit-building.component'
import {PlanRobotRouteComponent} from './home/tasks/plan-robot-route/plan-robot-route.component'
import { EditElevatorsComponent } from './home/campus/edit-elevators/edit-elevators.component';
import { CreateRoomComponent } from "./home/campus/create-room/create-room.component";
import {PlanRouteWithViewComponent} from "./home/tasks/plan-route-with-view/plan-route-with-view.component"
import { PrivacyPolicyComponent } from './privacy-policy/privacy-policy.component';
import { CreateTaskComponent } from './home/tasks/create-task/create-task.component';
import { PendingTaskListComponent } from './home/tasks/pending-task-list/pending-task-list.component';

const routes: Routes = [
  {path: '', redirectTo: 'home', pathMatch:'full'},
  {path: 'login', component: LoginComponent},
  { path: 'register', component: RegisterComponent },
  { path: 'privacy-policy', component: PrivacyPolicyComponent },
  {
    path: 'home', component: HomeComponent, canActivate: [authGuard],
    children: [
      {
        path: 'users',
        component: UsersComponent
      },
      {
        path: 'users/create-user',
        component: CreateUserComponent,

      },
      {
        path: 'users/validate-user',
        component: ValidateUserComponent,

      },
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
        path: 'tasks/plan-route',
        component: PlanRobotRouteComponent,
      },
      {
        path: 'tasks/plan-route-with-view',
        component: PlanRouteWithViewComponent,
      },
      {
        path: 'tasks/create-task',
        component: CreateTaskComponent,
      },
      {
        path: 'tasks/pending-task-list',
        component: PendingTaskListComponent,
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
        path: 'campus/edit-building',
        component: EditBuildingComponent,
      },
      {
        path: 'campus/edit-bridge',
        component: EditBridgeComponent,
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
        path: 'campus/add-floor-map',
        component: AddMapFloorComponent,
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
        path: 'campus/list-elevators',
        component: ListElevatorsComponent,
      },
      {
        path: 'campus/edit-elevators',
        component: EditElevatorsComponent,
      },
      {
        path: 'campus/create-room',
        component: CreateRoomComponent,
      },
      {
        path: 'fleet/create-robot-type',
        component: CreateRobotTypeComponent,
      },
      {
        path: 'fleet/fleet-list',
        component: FleetListComponent,
      },
      {
        path: 'fleet/list-robots',
        component: RobotListComponent,
      },
      {
        path: 'fleet/create-robot',
        component: CreateRobotComponent,
      },


    ],
  },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
