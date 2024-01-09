import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../src/core/logic/Result';
import ITaskService from "../src/services/IServices/ITaskService";
import TaskController from "../src/controllers/taskController";
import ITaskDTO from '../src/dto/ITaskDTO';
import { Task } from '../src/domain/task';
import {TaskVigilance} from '../src/domain/task-agg/TaskVigilance';

describe('taskVigilance controller - Service', function () {
	const sandbox = sinon.createSandbox();


	beforeEach(function() {
		this.timeout(9000);
		Container.reset();

		let taskPickupSchemaInstance = require("../src/persistence/schemas/TaskPickupDeliverySchema").default;
		Container.set("taskPickupDeliverySchema", taskPickupSchemaInstance);

		let taskPickupRepoClass = require("../src/repos/TaskPickupDeliveryRepo").default;
		let taskPickupRepoInstance = Container.get(taskPickupRepoClass);
		Container.set("TaskPickupDeliveryRepo", taskPickupRepoInstance);

		let taskVigilanceSchemaInstance = require("../src/persistence/schemas/TaskVigilanceSchema").default;
		Container.set("taskVigilanceSchema", taskVigilanceSchemaInstance);

		let taskVigilanceRepoClass = require("../src/repos/TaskVigilanceRepo").default;
		let taskVigilanceRepoInstance = Container.get(taskVigilanceRepoClass);
		Container.set("TaskVigilanceRepo", taskVigilanceRepoInstance);

		let taskSchemaInstance = require("../src/persistence/schemas/TaskSchema").default;
		Container.set("taskSchema", taskSchemaInstance);

		let taskRepoClass = require("../src/repos/taskRepo").default;
		let taskRepoInstance = Container.get(taskRepoClass);
		Container.set("TaskRepo", taskRepoInstance);

		let taskServiceClass = require("../src/services/taskService").default;
		let taskServiceInstance = Container.get(taskServiceClass);
		Container.set("TaskService", taskServiceInstance);

    });

	afterEach(function() {
		sandbox.restore();
	});

    it('taskVigilanceController unit test using taskService stub', async function () {
		// Arrange
        let body = { "name":'task12' };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let taskServiceInstance = Container.get("TaskService");
		sinon.stub(taskServiceInstance, "createVigilanceTask").returns( Result.ok<ITaskDTO>( {"id":"123", "name": req.body.name} ));

		const ctrl = new TaskController(taskServiceInstance as ITaskService);

		// Act
		await ctrl.createVigilanceTask(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({ "id": "123","name": req.body.name}));
	});

    it('taskController + taskService integration test using taskRepoistory and Task stubs', async function () {
		// Arrange
        let body = { "name":'task12' };
        let req: Partial<Request> = {};
		req.body = body;

        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		sinon.stub(TaskVigilance, "create").returns(Result.ok({"id":"123", "description":"123", "buildingId": "12345", "floors":[{"floorNumber":1}],"startPosition":[1,2],"endPosition":[2,3],
		"contactNumber":13456,"user":{"name":"miguel"},"approved":false,"pending":false,"planned":false}));

		let taskVigilanceRepoInstance = Container.get("TaskVigilanceRepo");
		sinon.stub(taskVigilanceRepoInstance, "save").returns(new Promise<TaskVigilance>((resolve, reject) => {
			resolve(TaskVigilance.create({"description":"123", "buildingId": "12345", "floors":[{"floorNumber":1}],"startPosition":[1,2],"endPosition":[2,3],
		"contactNumber":13456,"user":{"name":"miguel"},"approved":false,"pending":false,"planned":false}).getValue())
		}));

		let taskServiceInstance = Container.get("TaskService");

		const ctrl = new TaskController(taskServiceInstance as ITaskService);

		// Act
		await ctrl.createVigilanceTask(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({"description":"123", "buildingId": "12345", "floors":[{"floorNumber":1}],"startPosition":[1,2],"endPosition":[2,3],
		"contactNumber":13456,"user":{"name":"miguel"},"approved":false,"pending":false,"planned":false}));
	});


});


