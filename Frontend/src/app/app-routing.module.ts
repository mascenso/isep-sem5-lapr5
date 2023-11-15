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
import {BridgeListComponent} from "./home/campus/bridge-list/bridge-list.component";
import { ListBuildingFloorsComponent } from './home/campus/list-building-floors/list-building-floors.component';
import {RobotListComponent} from "./home/fleet/robot-list/robot-list.component";

const routes: Routes = [
  {path: '', redirectTo: 'home', pathMatch:'full'},
  {
    path: 'home', component: HomeComponent,
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
        path: 'campus/list-floors',
        component: FloorListComponent,
      },
      {
        path: 'campus/list-bridges',
        component: BridgeListComponent,
      },
      {
        path: 'campus/list-building-floors',
        component: ListBuildingFloorsComponent,
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
  { path: '**', redirectTo: ''}
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
