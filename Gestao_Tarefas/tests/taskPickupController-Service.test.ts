import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../src/core/logic/Result';
import ITaskService from "../src/services/IServices/ITaskService";
import TaskController from "../src/controllers/taskController";
import ITaskDTO from '../src/dto/ITaskDTO';
import { Task } from '../src/domain/task';
import {TaskPickupDelivery} from '../src/domain/task-agg/TaskPickupDelivery';
import { LocationRoom } from '../src/domain/task-agg/locationRoom';
import { User } from '../src/domain/task-agg/user';
import { TaskStatusVO } from "../src/domain/task-agg/taskStatusVO";
import ITaskPickupDeliveryDTO from "../src/dto/ITaskPickupDeliveryDTO";

describe('TaskPickupDelivery controller - Service', function () {
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
/*
    it('TaskPickupDeliveryController unit test using taskService stub', async function () {
		// Arrange
        let body = { "name":'task12' };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let taskServiceInstance = Container.get("TaskService");
		sinon.stub(taskServiceInstance, "createPickupDeliveryTask").returns( Result.ok<ITaskPickupDeliveryDTO>);

		const ctrl = new TaskController(taskServiceInstance as ITaskService);

		// Act
		await ctrl.createPickupDeliveryTask(<Request>req, <Response>res, <NextFunction>next);

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

		sinon.stub(TaskPickupDelivery, "create").returns(Result.ok({
			approved: false,
			contactNumber: 13456,
			deliveryContact: "987654321",
			deliveryLocalization: [1,2],
			description: "123",
			id: "123",
			pending: false,
			pickupContact: "987654321",
			pickupLocalization: [7,2],
			planned: false,
			user: { name: "miguel" }
		  }));

		let TaskPickupDeliveryRepoInstance = Container.get("TaskPickupDeliveryRepo");
		sinon.stub(TaskPickupDeliveryRepoInstance, "save").returns(new Promise<TaskPickupDelivery>((resolve, reject) => {
			resolve(TaskPickupDelivery.create({
				"description":"123",
				"pickupLocalization": LocationRoom.create("buildingId",{"name":1},[1,1]).getValue(),
				"deliveryLocalization":LocationRoom.create("buildingId",{"name":1},[1,1]).getValue(),
				"contactNumber":13456,
				"user": User.create("Miguel",987654321, 'a@email.pt').getValue(),
				"deliveryContact":User.create("Miguel",987654321, 'b@email.pt').getValue(),
				"pickupContact":User.create("Miguel",987654321, 'c@email.pt').getValue(),
        "taskStatus": TaskStatusVO.create(true, false, false).getValue()
              }).getValue())
		}));

		let taskServiceInstance = Container.get("TaskService");

		const ctrl = new TaskController(taskServiceInstance as ITaskService);

		// Act
		await ctrl.createPickupDeliveryTask(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({
			approved: false,
			contactNumber: 13456,
			deliveryContact: "987654321",
			deliveryLocalization: [1, 2],
			description: "123",
			id: "123",
			pending: false,
			pickupContact: "987654321",
			pickupLocalization: [7, 2],
			taskStatus: { approved: false, pending: false, planned: false },
			user: { name: "miguel" }
		  }));
	});

*/
});


