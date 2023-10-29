import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import IBuildingService from "../../src/services/IServices/IBuildingService";
import BuildingController from "../../src/controllers/buildingController";
import {IBuildingDTO} from '../../src/dto/IBuildingDTO';
import { Building } from '../../src/domain/building';

describe('Integration test building controller -> service ', function () {
	const sandbox = sinon.createSandbox();

	beforeEach(function() {
		this.timeout(9000);
		Container.reset();
		let buildingSchemaInstance = require("../../src/persistence/schemas/buildingSchema").default;
		Container.set("buildingSchema", buildingSchemaInstance);

		let buildingRepoClass = require("../../src/repos/buildingRepo").default;
		let buildingRepoInstance = Container.get(buildingRepoClass);
		Container.set("BuildingRepo", buildingRepoInstance);

		let buildingInstance = require("../../src/domain/building").Building;
		Container.set("Building", buildingInstance);

		let floorInstance = require("../../src/domain/floor").default;
		Container.set("Floor", floorInstance);

		let buildingServiceClass = require("../../src/services/buildingService").default;
		let buildingServiceInstance = Container.get(buildingServiceClass);
		Container.set("BuildingService", buildingServiceInstance);

    });

	afterEach(function() {
		sinon.restore();
		sandbox.restore();
	});

    it('BuildingController -> buildingService integration test using buildingRepo and Building stub (createBuilding)', async function () {
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingRepoInstance = Container.get("BuildingRepo");
		sinon.stub(buildingRepoInstance, "save").returns(new Promise<Building>((resolve, reject) => {
			resolve(Building.create({
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}).getValue())
		}));

		let buildingInstance = Container.get("Building");
		const buildingStub = sinon.stub(buildingInstance, "create").returns( Result.ok( {
			"id":"123",
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description}
		));


		let buildingServiceInstance = Container.get('BuildingService');
		const ctrl = new BuildingController(buildingServiceInstance as IBuildingService);

		// Act
		await ctrl.createBuilding(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(res.json);
		sinon.assert.calledWith(res.json, sinon.match({
			"id":"123",
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description
		}));
		sinon.assert.calledOnce(buildingStub);
	});

	it('BuildingController -> buildingService integration test using buildingRepo and Building stub (updateBuilding)', async function () {
		// Arrange
        let body = { "id":"123","code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingRepoInstance = Container.get("BuildingRepo");
		const saveStub =  sinon.stub(buildingRepoInstance, "save").returns(new Promise<Building>((resolve, reject) => {
			resolve(Building.create({
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}).getValue())
		}));

		const findByDomainIdStub = sinon.stub(buildingRepoInstance, "findByDomainId").returns(new Promise<Building[]>((resolve, reject) => {
			resolve([Building.create({
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}).getValue()])
		}));


		let buildingServiceInstance = Container.get('BuildingService');
		const ctrl = new BuildingController(buildingServiceInstance as IBuildingService);

		// Act
		await ctrl.updateBuilding(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(findByDomainIdStub);
		sinon.assert.calledWith(findByDomainIdStub, sinon.match("123"));
		sinon.assert.calledOnce(saveStub);
		sinon.assert.calledWith(saveStub,sinon.match({
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description}));
	});

	it('BuildingController -> buildingService integration test using buildingRepo and Building stub (getAllBuildings)', async function () {
		// Arrange
        let body = { "id":"123","code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;
        let res: Partial<Response> = {
			json: sinon.spy()
        };
		let next: Partial<NextFunction> = () => {};

		let buildingRepoInstance = Container.get("BuildingRepo");
		const getAllBuildingsStub  =  sinon.stub(buildingRepoInstance, "getAllBuildings").returns(new Promise<Building[]>((resolve, reject) => {
			resolve([Building.create({
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}).getValue()])
		}));


		let buildingServiceInstance = Container.get('BuildingService');
		const ctrl = new BuildingController(buildingServiceInstance as IBuildingService);

		// Act
		await ctrl.getAllBuildings(<Request>req, <Response>res, <NextFunction>next);

		// Assert
		sinon.assert.calledOnce(getAllBuildingsStub);
	});

});


