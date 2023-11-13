import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import {HomeComponent} from "./home/home.component";
import {CampusComponent} from "./home/campus/campus.component";
import {FleetComponent} from "./home/fleet/fleet.component";
import {TasksComponent} from "./home/tasks/tasks.component";
import {LoginComponent} from "./login/login.component";
import {CreateBuildingComponent} from "./home/campus/create-building/create-building.component";
import {CreateRobotTypeComponent} from "./home/fleet/create-robot-type/create-robot-type.component";
import {FloorListComponent} from "./home/campus/floor-list/floor-list.component";
import {BridgeListComponent} from "./home/campus/bridge-list/bridge-list.component";

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
        path: 'fleet/create-robot-type',
        component: CreateRobotTypeComponent,
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
